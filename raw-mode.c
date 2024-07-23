#include <termios.h>
#include <unistd.h>

struct termios orig_termios; 

void disableRawMode(void) {
	tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
}

void enableRawMode(void) {
	tcgetattr(STDIN_FILENO, &orig_termios);

	struct termios raw = orig_termios;

	raw.c_iflag &= ~(BRKINT | INPCK | ISTRIP | IXON | ICRNL);
	raw.c_oflag &= ~(OPOST);
	raw.c_cflag |= (CS8);
	raw.c_lflag &= ~(ECHO | ICANON | IEXTEN| ISIG);

	tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}
