extern crate libc;
extern crate termios;

use libc::{winsize, STDIN_FILENO, STDOUT_FILENO, TIOCGWINSZ};
use std::{env, fs, io, io::*};
use termios::{tcsetattr, Termios, TCSAFLUSH};

// constants

const VERSION: &str = env!("CARGO_PKG_VERSION");

macro_rules! ctrl_key {
    ($k:expr) => {
        $k & 0x1f
    };
}

const CTRL_Q: u8 = ctrl_key!(b'q');

#[derive(PartialEq, Clone, Copy, Debug)]
enum Key {
    Character(u8),
    ArrowUp,
    ArrowDown,
    ArrowLeft,
    ArrowRight,
    PageUp,
    PageDown,
    Home,
    End,
    Delete,

    NoKey,
}

/*** terminal ***/

struct RawMode {
    orig_term: Termios,
}

impl RawMode {
    fn enable_raw_mode() -> io::Result<Self> {
        use termios::*;

        let mut term = Termios::from_fd(STDIN_FILENO)?;
        let mode = RawMode { orig_term: term };

        term.c_iflag &= !(ICRNL | IXON | BRKINT | INPCK | ISTRIP);
        term.c_oflag &= !(OPOST);
        term.c_cflag |= CS8;
        term.c_lflag &= !(ECHO | ICANON | ISIG | IEXTEN);
        term.c_cc[VMIN] = 0;
        term.c_cc[VTIME] = 1;

        tcsetattr(STDIN_FILENO, TCSAFLUSH, &term)?;

        Ok(mode)
    }
}

impl Drop for RawMode {
    fn drop(&mut self) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &self.orig_term).expect("Failed to drop raw mode");
    }
}

struct Row {
    chars: String,
    render: String,
}

impl Row {
    pub fn new() -> Row {
        Row {
            chars: String::new(),
            render: String::new(),
        }
    }
}

fn get_window_size() -> io::Result<(u16, u16)> {
    let ws = winsize {
        ws_col: 0,
        ws_row: 0,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };

    unsafe {
        if libc::ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 01 || ws.ws_col == 0 {
            return Err(Error::new(
                ErrorKind::Other,
                "get_window_size: ioctl failed",
            ));
        }
    }

    Ok((ws.ws_row, ws.ws_col))
}

struct Editor {
    stdin: io::Stdin,
    stdout: io::Stdout,
    screen_rows: u16,
    screen_cols: u16,
    cursor_x: u16,
    cursor_y: u16,
    rows: Vec<Row>,
}

impl Editor {
    fn new() -> io::Result<Editor> {
        let (rows, cols) = get_window_size()?;
        Ok(Editor {
            stdin: io::stdin(),
            stdout: io::stdout(),
            screen_cols: cols,
            screen_rows: rows,
            cursor_x: 0,
            cursor_y: 0,
            rows: vec![Row::new()],
        })
    }

    fn open(&mut self, filename: &String) {
        let content = fs::read_to_string(filename).expect("Something went wrong reading the file");

        let lines = content.lines().collect::<Vec<_>>();

        self.rows[0].chars = lines[0].to_string();
        self.rows[0].chars.push('\0');
    }

    fn read(&mut self, buf: &mut [u8]) -> usize {
        self.stdin.read(buf).expect("Failed to read buffer")
    }

    fn read_key(&mut self) -> Option<Key> {
        let mut buf = [0; 1];
        self.read(&mut buf);

        if &buf == b"\x1b" {
            // ANSI Escape Sequence

            let mut seq = [0; 1];
            let mut last = [0; 1];
            self.read(&mut seq);
            match seq[0] {
                b'[' => {
                    self.read(&mut seq);
                    match seq[0] {
                        s if s >= b'0' && s <= b'9' => {
                            self.read(&mut last);
                            match last[0] {
                                b'~' => match seq[0] {
                                    b'1' => Some(Key::Home),

                                    b'3' => Some(Key::Delete),

                                    b'4' => Some(Key::End),
                                    b'5' => Some(Key::PageUp),
                                    b'6' => Some(Key::PageDown),
                                    b'7' => Some(Key::Home),
                                    b'8' => Some(Key::End),

                                    _ => None,
                                },

                                _ => None,
                            }
                        }
                        b'A' => Some(Key::ArrowUp),
                        b'B' => Some(Key::ArrowDown),
                        b'C' => Some(Key::ArrowRight),
                        b'D' => Some(Key::ArrowLeft),
                        b'H' => Some(Key::Home),
                        b'F' => Some(Key::End),

                        _ => None,
                    }
                }

                b'O' => {
                    self.read(&mut seq);
                    match seq[0] {
                        b'H' => Some(Key::Home),
                        b'F' => Some(Key::Home),

                        _ => None,
                    }
                }

                _ => None,
            }
        } else {
            Some(Key::Character(buf[0]))
        }
    }

    fn process_key(&mut self) -> bool {
        let key = self.read_key().unwrap_or(Key::NoKey);

        match key {
            Key::Character(value) => match value {
                CTRL_Q => false,

                _ => true,
            },

            Key::ArrowDown | Key::ArrowUp | Key::ArrowLeft | Key::ArrowRight => {
                self.move_cursor(key)
            }

            Key::PageUp => {
                for _ in 0..self.screen_rows {
                    self.move_cursor(Key::ArrowUp);
                }
                true
            }

            Key::PageDown => {
                for _ in 0..self.screen_cols {
                    self.move_cursor(Key::ArrowDown);
                }
                true
            }

            Key::Home => {
                self.cursor_x = 0;

                true
            }

            Key::End => {
                self.cursor_x = self.screen_cols - 1;

                true
            }

            _ => true,
        }
    }

    fn move_cursor(&mut self, key: Key) -> bool {
        match key {
            Key::ArrowUp => {
                if self.cursor_y > 0 {
                    self.cursor_y -= 1;
                }
            }
            Key::ArrowDown => {
                if self.cursor_y < self.screen_rows - 1 {
                    self.cursor_y += 1;
                }
            }
            Key::ArrowLeft => {
                if self.cursor_x > 0 {
                    self.cursor_x -= 1;
                }
            }
            Key::ArrowRight => {
                if self.cursor_x < self.screen_cols - 1 {
                    self.cursor_x += 1;
                }
            }

            _ => (),
        }

        true
    }

    fn write(&mut self, buffer: &[u8]) -> io::Result<()> {
        self.stdout.write_all(buffer)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stdout.flush()
    }

    fn draw_rows(&mut self, pending_write: &mut Vec<u8>) -> io::Result<()> {
        for y in 0..self.screen_rows {
            if y as usize >= self.rows.len() {
                if y == self.screen_rows / 3 {
                    let welcome: String = format!("Kilo Editor -- version {}", VERSION);
                    let mut truncated = welcome
                        .as_bytes()
                        .iter()
                        .take(self.screen_cols as usize)
                        .cloned()
                        .collect::<Vec<_>>();

                    let mut padding = (self.screen_cols as usize - truncated.len()) / 2;
                    if padding > 0 {
                        pending_write.append(&mut b"~".to_vec());
                        padding -= 1;
                    }

                    for _ in 0..padding {
                        pending_write.append(&mut b" ".to_vec());
                    }

                    pending_write.append(&mut truncated);
                } else {
                    pending_write.append(&mut b"~".to_vec());
                }
            } else {
                let mut text = self.rows[0]
                    .chars
                    .as_bytes()
                    .iter()
                    .take(self.screen_cols as usize)
                    .cloned()
                    .collect::<Vec<_>>();
                pending_write.append(&mut text);
            }

            pending_write.append(&mut b"\x1b[K".to_vec());

            if y < self.screen_rows - 1 {
                pending_write.append(&mut b"\r\n".to_vec());
            }
        }

        Ok(())
    }

    fn refresh_screen(&mut self) -> io::Result<()> {
        let mut pending_write = vec![];

        pending_write.append(&mut b"\x1b[?25l".to_vec());
        pending_write.append(&mut b"\x1b[H".to_vec());

        self.draw_rows(&mut pending_write)?;

        let cursor_cmd: String = format!("\x1b[{};{}H", self.cursor_y + 1, self.cursor_x + 1);

        pending_write.append(&mut cursor_cmd.as_bytes().to_vec());
        pending_write.append(&mut b"\x1b[?25h".to_vec());

        self.write(&pending_write)?;

        self.flush()
    }

    fn clear_screen(&mut self) -> io::Result<()> {
        self.stdout.write_all(b"\x1b[2J")?;
        self.stdout.write_all(b"\x1b[H")?;

        self.flush()
    }
}

impl Drop for Editor {
    fn drop(&mut self) {
        self.clear_screen().expect("Failed to clear screen");
    }
}

/*** init ***/

fn main() -> io::Result<()> {
    let mut editor = Editor::new()?;
    let _mode = RawMode::enable_raw_mode().unwrap();

    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        editor.open(&args[1]);
    }

    'main: loop {
        editor.refresh_screen()?;
        if !editor.process_key() {
            break 'main;
        }
    }

    Ok(())
}
