/* Includes */
#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

/* Header files */
#include <unistd.h>
#include <termios.h>
#include <stdarg.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <fcntl.h>

/* Defines */
#define CTRL_KEY(k) ((k) & 0x1f)

#define SCREEV_VERSION "0.01"
#define SCREEV_TAB_STOP 8
#define SCREEV_QUIT 2 // No. of times needed to quit unsaved work

#define LINE_NUMBER_ENDPOS 2

enum editorKey { // Key maps
	BACKSPACE = 127, // Backspace has no human-readable escape key so we assign it 127
	ARROW_LEFT = 1000, // value of 1000 is so that it out of range of WASD
	ARROW_RIGHT,
	ARROW_UP,
	ARROW_DOWN,
	DEL_KEY,
	HOME_KEY,
	END_KEY,
	PAGE_UP,
	PAGE_DOWN
};

enum editorHighlight { // For highlighting strings/chars
	HL_NORMAL = 0,
	HL_COMMENT,
	HL_MLCOMMENT,
	HL_KEYWORD1,
	HL_KEYWORD2,
	HL_STRING,
	HL_NUMBER,
	HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/* Data structs */
struct editorSyntax { // Used for colouring different file types
	char *filetype;
	char **filematch;
	char **keywords;
	char *singleline_comment_start;
	char *multiline_comment_start;
	char *multiline_comment_end;
	int flags;
};

struct termios orig_termios;

typedef struct erow { // Storing row of text
	int idx;
	int size;
	int rsize;
	char *chars;
	char *render;
	unsigned char *hl;
	int hl_open_comment;
} erow;

struct editorConfig { // Stores editor state for window size
	int cx, cy; // Cursor coords
	int rx;
	int rowoff; // row offset for scrolling
	int coloff; // col offset
	int screenrows;
	int screencols;
	int numrows;
	erow *row;
	int dirty; // 'dirty' flag for changes in file
	char *filename;
	char statusmsg[80];
	time_t statusmsg_time;
	struct editorSyntax *syntax;
	struct termios orig_termios;
};

struct editorConfig E;

/* Filetypes */
char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
	"switch", "if", "while", "for", "break", "continue", "return", "else",
	"struct", "union", "typedef", "static", "enum", "class", "case",
	"int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
	"void|", NULL // C keywords
};

struct editorSyntax HLDB[] = {
	{
		"c",
		C_HL_extensions,
		C_HL_keywords,
		"//", "/*", "*/",
		HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
	},
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/* Function Prototypes */
void editorSetStatusMessage(const char *fmt, ...); // Needed because of implicit 
												   // declaration in editorSave
void editorRefreshScreen(); // Needed for editorPrompt
char *editorPrompt(char *prompt, void (*callback)(char *, int));

/* Combine *char */
char *concat(char *str1, char *str2)
{
	int newSize = strlen(str1) + strlen(str2) + 1;
	char * newBuffer = (char *)malloc(newSize);
	
	strcpy(newBuffer, str1);
	strcat(newBuffer, str2);
	
	return newBuffer;
}

/* Terminal */
void die(const char *s) // Error handler function
{
	write(STDOUT_FILENO, "\x1b[2J", 4); // Write escape sequence to terminal
	write(STDOUT_FILENO, "\x1b[H", 3); // Position cursor top left
	
	perror(s);
	exit(1);
}

void disableRawMode() // Restore terminal attributes when done
{
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
		die("tcgetattr");
}

void enableRawMode()
{
	if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
	atexit(disableRawMode); // Disable Raw Mode at exit
	
	struct termios raw = E.orig_termios;
	// Disable certain flags so that input may be read properly
	raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
	raw.c_oflag &= ~(OPOST);
	raw.c_cflag |= (CS8);
	raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
	raw.c_cc[VMIN] = 0; // Set a timeout for read()
	raw.c_cc[VTIME] = 1;
	
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcgetattr");
}

int editorReadKey() // Reads key press
{
	int nread;
	char c;
	while ((nread = read(STDIN_FILENO, &c, 1)) != 1)
	{
		if (nread == -1 && errno != EAGAIN) die("read");
	}
	
	if (c == '\x1b') { // Mapping arrows keys so that cursor moves
		char seq[3];
		
		if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
		if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';
		
		if (seq[0] == '[') {
		if (seq[1] >= '0' && seq[1] <= '9'){
				if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
				if (seq[2] == '~') {
					switch (seq[1]) {
						case '1': return HOME_KEY;
						case '3': return DEL_KEY;
						case '4': return END_KEY;
						case '5': return PAGE_UP;
						case '6': return PAGE_DOWN;
						case '7': return HOME_KEY;
						case '8': return END_KEY;
					}
				}
		} else {
			switch (seq[1]) {
				case 'A': return ARROW_UP;
				case 'B': return ARROW_DOWN;
				case 'C': return ARROW_RIGHT;
				case 'D': return ARROW_LEFT;
				case 'H': return HOME_KEY;
				case 'F': return END_KEY;
			}
		}
	} else if (seq[0] == 'O') {
		switch (seq[1]) {
			case 'H': return HOME_KEY;
			case 'F': return END_KEY;
		}
	}
		
		return '\x1b';
	} else {
	return c;
	}
}

int getCursorPosition(int *rows, int *cols) 
{
  char buf[32];
  unsigned int i = 0;
  
  if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;
  
  while (i < sizeof(buf) - 1) {
    if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
    if (buf[i] == 'R') break;
    i++;
  }
  buf[i] = '\0';
  
  if (buf[0] != '\x1b' || buf[1] != '[') return -1;
  if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;
  
  return 0;
}

int getWindowSize( int *rows, int *cols)
{
	struct winsize ws;
	
	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0)
	{
		if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
		editorReadKey();
		return getCursorPosition(rows, cols);
	} else {
		*cols = ws.ws_col;
		*rows = ws.ws_row;
		return 0;
	}
}

/* Syntax Highlighting */
int is_separator(int c) // Checks characters for separators
{
	return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row)
{
	row->hl = realloc(row->hl, row->rsize);
	memset(row->hl, HL_NORMAL, row->rsize);
	
	if (E.syntax == NULL) return; // Return if no filetype
	
	char **keywords = E.syntax->keywords;
	
	char *scs = E.syntax->singleline_comment_start;
	char *mcs = E.syntax->multiline_comment_start;
	char *mce = E.syntax->multiline_comment_end;
	
	int scs_len = scs ? strlen(scs) : 0;
	int mcs_len = mcs ? strlen(mcs) : 0;
	int mce_len = mce ? strlen(mce) : 0;
	
	int prev_sep = 1; // Flags for checking syntax
	int in_string = 0;
	int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);
	
	int i = 0;
	while (i < row->rsize) {
		char c = row->render[i];
		unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;
		
		if (scs_len && !in_string && !in_comment) {
			if (!strncmp(&row->render[i], scs, scs_len)) {
				memset(&row->hl[i], HL_COMMENT, row->rsize - i); // Comment highlighting (full lines)
				break;
			}
		}
		
		
		if (mcs_len && mce_len && !in_string) { // Multiple line comment highlighting
			if (in_comment) {
				row->hl[i] = HL_MLCOMMENT;
				if (!strncmp(&row->render[i], mce, mce_len)) {
				memset(&row->hl[i], HL_MLCOMMENT, mce_len);
				i += mce_len;
				in_comment = 0;
				prev_sep = 1;
				continue;
				} else {
				i++;
				continue;
				}
			} else if (!strncmp(&row->render[i], mcs, mcs_len)) {
				memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
				i += mcs_len;
				in_comment = 1;
				continue;
			}
		}
		
		if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
			if (in_string) {
				row->hl[i] = HL_STRING;
				if (c == '\\' && i + 1 < row->rsize) { // Comment highlighting (single chars)
				row->hl[i + 1] = HL_STRING;
				i += 2;
				continue;
				}
				if (c == in_string) in_string = 0;
				i++;
				prev_sep = 1;
				continue;
			} else {
			if (c == '"' || c == '\'') { // Strings highlighting
				in_string = c;
				row->hl[i] = HL_STRING;
				i++;
				continue;
			}
		}
    }
		
		if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
			if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
				(c == '.' && prev_hl == HL_NUMBER)) {
				row->hl[i] = HL_NUMBER;
				i++;
				prev_sep = 0;
				continue;
			}
		}
		
		if (prev_sep) { // Makes sure keyword is highlighted properly
			int j;
			for (j = 0; keywords[j]; j++) {
				int klen = strlen(keywords[j]);
				int kw2 = keywords[j][klen - 1] == '|';
				if (kw2) klen--;
				if (!strncmp(&row->render[i], keywords[j], klen) &&
					is_separator(row->render[i + klen])) {
				memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
				i += klen;
				break;
				}
			}
			if (keywords[j] != NULL) {
				prev_sep = 0;
				continue;
			}
		}
		
		prev_sep = is_separator(c);
		i++;
	}
	
	// Allows us to continue propogating ML comment highlighting
	int changed = (row->hl_open_comment != in_comment); 
	row->hl_open_comment = in_comment;
	if (changed && row->idx + 1 < E.numrows)
		editorUpdateSyntax(&E.row[row->idx + 1]);
}

int editorSyntaxToColour(int hl)
{
	switch(hl) {
		case HL_COMMENT: 
		case HL_MLCOMMENT: return 36; // Integers = terminal colours 
		case HL_KEYWORD1: return 33;
		case HL_KEYWORD2: return 32;
		case HL_STRING: return 35;
		case HL_NUMBER: return 31;
		case HL_MATCH: return 34;
		default: return 37;
	}
}

void editorSelectSyntaxHighlight() // Matches filetype to database
{
	E.syntax = NULL;
	if (E.filename == NULL) return;
	char *ext = strrchr(E.filename, '.');
	for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
		struct editorSyntax *s = &HLDB[j];
		unsigned int i = 0;
		while (s->filematch[i]) {
		int is_ext = (s->filematch[i][0] == '.');
		if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
			(!is_ext && strstr(E.filename, s->filematch[i]))) {
			E.syntax = s;
			
			int filerow;
			for (filerow = 0; filerow < E.numrows; filerow++) {
				editorUpdateSyntax(&E.row[filerow]);
			}
			
			return;
		}
		i++;
		}
	}
}

/* Row Operations */
int editorRowCxToRx(erow *row, int cx) // Allows tabs to be properly displayed
{
	int rx = 0;
	int j;
	for (j = 0; j < cx; j++) {
		if ( row->chars[j] == '\t')
			rx += (SCREEV_TAB_STOP - 1) - (rx % SCREEV_TAB_STOP);
		rx++;
	}
	return rx;
}


int editorRowRxToCx(erow *row, int rx) // Opposite to above function
{
	int cur_rx = 0;
	int cx;
	for (cx = 0; cx < row->size; cx++) {
		if (row->chars[cx] == '\t')
		cur_rx += (SCREEV_TAB_STOP - 1) - (cur_rx % SCREEV_TAB_STOP);
		cur_rx++;
		if (cur_rx > rx) return cx;
	}
	return cx;
}

void editorUpdateRow(erow *row) 
{
	int tabs = 0;
	int j;
	for (j = 0; j < row->size; j++)
		if (row->chars[j] == '\t') tabs++;
		
	free(row->render);
	row->render = malloc(row->size + tabs*(SCREEV_TAB_STOP - 1) + 1);

	int idx = 0;
	for (j = 0; j < row->size; j++) {
		if (row->chars[j] == '\t') {
		  row->render[idx++] = ' ';
		  while (idx % SCREEV_TAB_STOP != 0) row->render[idx++] = ' ';
		} else {
		  row->render[idx++] = row->chars[j];
		}
	}
	row->render[idx] = '\0';
	row->rsize = idx;
	
	editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len)
{
	if (at < 0 || at > E.numrows) return;
	
	E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
	memmove(&E.row[at +1], &E.row[at], sizeof(erow) * (E.numrows - at));
	for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++;
	
	E.row[at].idx = at;
	
	E.row[at].size = len;
	E.row[at].chars = malloc(len + 1);
	memcpy(E.row[at].chars, s, len);
	E.row[at].chars[len] = '\0';
	
	E.row[at].rsize = 0;
	E.row[at].render = NULL;
	E.row[at].hl = NULL;
	E.row[at].hl_open_comment = 0;
	editorUpdateRow(&E.row[at]);
	
	E.numrows++;
	E.dirty++;
}

void editorFreeRow(erow *row) // Freeing memory of deleted row
{
	free(row->render);
	free(row->chars);
	free(row->hl);
}

void editorDelRow(int at)
{
	if (at < 0 || at >= E.numrows) return;
	editorFreeRow(&E.row[at]);
	memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
	for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--;
	E.numrows--;
	E.dirty++;
}

void editorRowInsertChar(erow *row, int at, int c) // Allows editing
{
	if (at < 0 || at > row->size) at = row->size;
	row->chars = realloc(row->chars, row->size + 2);
	memmove(&row->chars[at + 1], concat("~ |",&row->chars[at]), row->size - at + 1);
	row->size++;
	row->chars[at] = c;
	editorUpdateRow(row);
	E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len) // Appends string to end of row
{
	row->chars = realloc(row->chars, row->size + len + 1);
	memcpy(&row->chars[row->size], s, len);
	row->size += len;
	row->chars[row->size] = '\0';
	editorUpdateRow(row);
	E.dirty++;
}

void editorRowDelChar(erow *row, int at) // Backspace function
{
	if (at < 0 || at >= row->size) return;
	memmove(&row->chars[at], &row->chars[at + 1], row->size - at + LINE_NUMBER_ENDPOS);
	row->size--;
	editorUpdateRow(row);
	E.dirty++;
}

/* Editor Operations */
void editorInsertChar(int c)
{
	if (E.cy == E.numrows) {
		editorInsertRow(E.numrows, "~|", LINE_NUMBER_ENDPOS);
	}
	editorRowInsertChar(&E.row[E.cy], E.cx + LINE_NUMBER_ENDPOS, c);
	E.cx++;
}

void editorInsertNewLine() // Handles 'Enter'
{
	if (E.cx == LINE_NUMBER_ENDPOS) {
		editorInsertRow(E.cy, "~|", LINE_NUMBER_ENDPOS);
	} else {
		erow *row = &E.row[E.cy];
		editorInsertRow(E.cy + 1, concat("~|",&row->chars[E.cx]), row->size - E.cx + LINE_NUMBER_ENDPOS );
		row = &E.row[E.cy];
		row->size = E.cx;
		row->chars[row->size] = '\0';
		editorUpdateRow(row);
	}
	E.cy++;
	E.cx = LINE_NUMBER_ENDPOS;
}

void editorDelChar() // Backspace function
{
	if (E.cy == E.numrows) return;
	if (E.cx == LINE_NUMBER_ENDPOS && E.cy == 0) return;
	
	erow *row = &E.row[E.cy];
	if (E.cx > LINE_NUMBER_ENDPOS) {
		editorRowDelChar(row, E.cx - 1);
		E.cx--;
	} else { // Handles case of cursor at start of line
		E.cx = E.row[E.cy - 1].size;
		editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size + LINE_NUMBER_ENDPOS);
		editorDelRow(E.cy);
		E.cy--;
	}
}

/* File I/O */
char *editorRowsToString(int *buflen) // Converts rows to strings for writing to file
{
	int totlen = 0;
	int j;
	for (j = 0; j < E.numrows; j++)
		totlen += E.row[j].size + 1;
	*buflen = totlen;
	char *buf = malloc(totlen);
	char *p = buf;
	for (j = 0; j < E.numrows; j++) {
		memcpy(p, E.row[j].chars, E.row[j].size);
		p += E.row[j].size;
		*p = '\n';
		p++;
	}
	
	return buf;
}

void editorOpen(char *filename) // Allows opening of text files
{
	free(E.filename);
	E.filename = strdup(filename);
	
	editorSelectSyntaxHighlight();
	
	FILE *fp = fopen(filename, "r");
	if (!fp) die("fopen");
	
	char *line = NULL;
	size_t linecap = 0;
	ssize_t linelen;
	while ((linelen = getline(&line, &linecap, fp)) != -1) {
		while (linelen > 0 && (line[linelen - 1] == '\n' ||
							   line[linelen - 1] == '\r'))
			linelen--;	
	editorInsertRow(E.numrows, line, linelen);
	}
	free(line);
	fclose(fp);
	E.dirty = 0;
}

void editorSave() // Saving and writing new files
{
	if (E.filename == NULL) {
		E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
		if (E.filename == NULL) {
		editorSetStatusMessage("Save aborted");
		return;
		}
		editorSelectSyntaxHighlight();
	}
	
	if (E.filename == NULL) return;
	int len;
	char *buf = editorRowsToString(&len);
	int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
	
	if (fd != -1) { // Error save handling
		if (ftruncate(fd, len) != -1) {
		  if (write(fd, buf, len) == len) {
			close(fd);
			free(buf);
			E.dirty = 0;
			editorSetStatusMessage("%d bytes written to disc", len);
			return;
		  }
		}
		close(fd);
	}
	
	free(buf);	
	editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

/* Search function */
void editorSearchCallback(char *query, int key) // Incremental Search
{
	static int last_match = -1;
	static int direction = 1;
	
	static int saved_hl_line;
	static char *saved_hl = NULL;
	
	if (saved_hl) {
		memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
		free(saved_hl);
		saved_hl = NULL;
	}
  
	if (key == '\r' || key == '\x1b') {
		last_match = -1;
		direction = 1;
		return;
	} else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
		direction = 1;
	} else if (key == ARROW_LEFT || key == ARROW_UP) {
		direction = -1;
	} else {
		last_match = -1;
		direction = 1;
	}
	
	if (last_match == -1) direction = 1; // Allows movement through rows
	int current = last_match;
	int i;
	for (i = 0; i < E.numrows; i++) {
		current += direction;
		if (current == -1) current = E.numrows - 1;
		else if (current == E.numrows) current = 0;
		erow *row = &E.row[current];
		char *match = strstr(row->render, query);
		if (match) {
		last_match = current;
		E.cy = current;
		E.cx = editorRowRxToCx(row, match - row->render);
		E.rowoff = E.numrows;
		
		saved_hl_line = current;
		saved_hl = malloc(row->rsize);
		memcpy(saved_hl, row->hl, row->rsize);
		memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
		break;
		}
	}
}

void editorSearch() 
{
	int saved_cx = E.cx; // Variables needed to return cursor
	int saved_cy = E.cy;
	int saved_coloff = E.coloff;
	int saved_rowoff = E.rowoff;
	
	char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)",
                             editorSearchCallback);
	
	if (query) { // ESC => cancel search 
	free(query);
	} else { // else return cursor
		E.cx = saved_cx;
		E.cy = saved_cy;
		E.coloff = saved_coloff;
		E.rowoff = saved_rowoff;
	}
}

/* Append Buffer */
struct abuf 
{
	char *b;
	int len;
};

#define ABUF_INIT {NULL, 0}

void abAppend(struct abuf *ab, const char *s, int len) // This ensures screen updates 
{													   // wholely at once
	char *new = realloc(ab->b, ab->len + len);
	
	if (new == NULL) return;
	memcpy(&new[ab->len], s, len);
	ab->b = new;
	ab->len += len;
}

void abFree(struct abuf *ab) // Free memory from append buffer
{
	free(ab->b);
}

/* Input */
char *editorPrompt(char *prompt, void (*callback)(char *, int)) 
{ // Used for prompts in status messages
	size_t bufsize = 128;
	char *buf = malloc(bufsize);
	size_t buflen = 0;
	buf[0] = '\0';
	while (1) {
		editorSetStatusMessage(prompt, buf);
		editorRefreshScreen();
		int c = editorReadKey();
		if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
			if ( buflen != 0) buf[--buflen] = '\0';
		} else if (c == '\x1b') {
		editorSetStatusMessage("");
		if (callback) callback(buf, c);
		free(buf);
		return NULL;
		} else if (c == '\r') {
		if (buflen != 0) {
			editorSetStatusMessage("");
			if (callback) callback(buf, c);
			return buf;
		}
		} else if (!iscntrl(c) && c < 128) {
		if (buflen == bufsize - 1) {
			bufsize *= 2;
			buf = realloc(buf, bufsize);
		}
		buf[buflen++] = c;
		buf[buflen] = '\0';
		}
		
		if (callback) callback(buf, c);
	}
}

void editorMoveCursor(int key) // Arrow keys to move cursor
{
	erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
	
	switch(key) {
		case ARROW_LEFT:
		if (E.cx != LINE_NUMBER_ENDPOS) {
		E.cx--;
		} else if (E.cy > 0) {
			E.cy--;
			E.cx = E.row[E.cy].size + LINE_NUMBER_ENDPOS;
		}
		break;
		case ARROW_RIGHT:
		if (row && E.cx < row->size) {
			E.cx++;
		} else if (row && E.cx == row->size) {
			E.cy++;
			E.cx = LINE_NUMBER_ENDPOS;
		}
		break;
		case ARROW_UP:
		if (E.cy != 0) {
		E.cy--;
		}
		break;
		case ARROW_DOWN:
		if (E.cy != E.numrows) {
		E.cx = LINE_NUMBER_ENDPOS;
		E.cy++;
		}
		break;
	}
	
	row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
	int rowlen = row ? row->size : 0;
	if (E.cx > rowlen) {
		E.cx = rowlen;
	}
}

void editorProcessKeyPress() // Handles key press
{
	static int quit = SCREEV_QUIT;
	int c = editorReadKey();
	
	switch (c)
	{
		case '\r':
		editorInsertNewLine();
		break;
		
		case CTRL_KEY('q'):
		if (E.dirty && quit > 0)
		{
			editorSetStatusMessage("WARNING! FILE HAS UNSAVED CHANGES."
				"Press Ctrl-Q %d more times to quit.", quit);
			quit--;
			return;
		}
		write(STDOUT_FILENO, "\x1b[2J", 4); // Write escape sequence to terminal
		write(STDOUT_FILENO, "\x1b[H", 3); // Position cursor top left
		printf("\033[2J\033[1;1H"); // Clear screen
		exit(0);
		break;
		
		case CTRL_KEY('s'): // CTRL-s to save
		editorSave();
		break;
		
		case HOME_KEY:
		E.cx = 0; // Move cursor to start of line
		break;
		
		case END_KEY:
		if (E.cy < E.numrows)
			E.cx = E.row[E.cy].size; // Move cursor to end of line
		break;
		
		case CTRL_KEY('f'): // Search function
		editorSearch();
		break;
		
		case BACKSPACE:
		case CTRL_KEY('h'):
		case DEL_KEY:
		if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
		editorDelChar();
		break;
		
		case PAGE_DOWN: // Move cursor to top/bottom of window
		case PAGE_UP:
		{
			if (c == PAGE_UP) {
				E.cy = E.rowoff;
			} else if (c == PAGE_DOWN) {
				E.cy = E.rowoff + E.screenrows - 1;
				if (E.cy > E.numrows) E.cy = E.numrows;
			}
			
			int times = E.screenrows;
			while (times--)
			editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
		}
		break;
		
		case ARROW_UP:
		case ARROW_DOWN:
		case ARROW_RIGHT:
		case ARROW_LEFT:
		editorMoveCursor(c);
		break;
		
		case CTRL_KEY('l'):
		case '\x1b':
		break;
		
		default:
		editorInsertChar(c);
		break;
	}
	
	quit = SCREEV_QUIT;
}

/* Output */
void editorScroll() 
{ 
	E.rx = LINE_NUMBER_ENDPOS;
	if (E.cy < E.numrows) {
		E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
	}
	
  if (E.cy < E.rowoff) {
    E.rowoff = E.cy;
  }
  if (E.cy >= E.rowoff + E.screenrows) {
    E.rowoff = E.cy - E.screenrows + 1;
  }
  if (E.rx < E.coloff) {
    E.coloff = E.cx;
  }
  if (E.rx >= E.coloff + E.screencols) {
    E.coloff = E.rx - E.screencols + 1;
  }
}

void editorDrawRows(struct abuf *ab) 
{
	int y;
	for (y = 0; y < E.screenrows; y++)
	{
		int filerow = y + E.rowoff;
		if (filerow >= E.numrows) {
			if (E.numrows == 0 && y == E.screenrows / 3)
			{
				char welcome[80]; // Draw welcome message
				int welcomelen = snprintf(welcome, sizeof(welcome),
				"Screev Editor -- version %s", SCREEV_VERSION);
				if (welcomelen > E.screencols) welcomelen = E.screencols;
				int padding = (E.screencols - welcomelen) / 2; // Centre welcome message
				if (padding) {
					abAppend(ab, "~|", LINE_NUMBER_ENDPOS);
					padding--;
				}
				while (padding--) abAppend(ab, " ", 1);
				abAppend(ab, welcome, welcomelen);
			} else {
			abAppend(ab, "~|", LINE_NUMBER_ENDPOS);
			}
		} else {
			int len = E.row[filerow].rsize - E.coloff;
			if (len < 0) len = 0;
			if (len > E.screencols) len = E.screencols;
			char *c = &E.row[filerow].render[E.coloff];
			unsigned char *hl = &E.row[filerow].hl[E.coloff];
			int current_colour = -1;
			int j;
			for (j = 0; j < len; j++) {
				if (iscntrl(c[j])) { // Check for ctrl keys
					char sym = (c[j] <= 26) ? '@' + c[j] : '?';
					abAppend(ab, "\x1b[7m", 4);
					abAppend(ab, &sym, 1);
					abAppend(ab, "\x1b[m", 3);
					if (current_colour != -1) {
						char buf[16];
						int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_colour);
						abAppend(ab, buf, clen);
					}
				} else if (hl[j] == HL_NORMAL) {
					if (current_colour != -1) {
						abAppend(ab, "\x1b[39m", 5);
						current_colour = -1;
					}
					abAppend(ab, &c[j], 1);
					} else {
					int colour = editorSyntaxToColour(hl[j]);
					if (colour != current_colour) {
						current_colour = colour;
						char buf[16];
						int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", colour);
						abAppend(ab, buf, clen);
					}	
					abAppend(ab, &c[j], 1);
					}
				}
			abAppend(ab, "\x1b[39m", 5);
		}
		
		abAppend(ab, "\x1b[K", 3); // Clear lines one at a time
		abAppend(ab, "\r\n", 2);
	}
}

void editorDrawStatusBar(struct abuf *ab) // Status bar to display filename + size
{
	abAppend(ab, "\x1b[7m", 4);
	char status[80], rstatus[80];
	int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
		E.filename ? E.filename : "[No Name]", E.numrows,
		E.dirty ? "(modified)" : "");
	int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
		E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);
	if (len > E.screencols) len = E.screencols;
	abAppend(ab, status, len);
	while (len < E.screencols) {
		if (E.screencols - len == rlen) {
			abAppend(ab, rstatus, rlen);
			break;
		} else {
			abAppend(ab, " ", 1);
			len++;
		}
	}
	abAppend(ab, "\x1b[m", 3);
	abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf *ab) {
	abAppend(ab, "\x1b[K", 3);
	int msglen =  strlen(E.statusmsg);
	if (msglen > E.screencols) msglen = E.screencols;
	if (msglen && time(NULL) - E.statusmsg_time < 5)
		abAppend(ab, E.statusmsg, msglen);
}

void editorRefreshScreen()
{
	editorScroll();
	struct abuf ab = ABUF_INIT;
	
	abAppend(&ab, "\x1b[?25l", 6); // Hide cursor when repainting screen
	abAppend(&ab, "\x1b[H", 3); // Position cursor top left
	
	editorDrawRows(&ab);
	editorDrawStatusBar(&ab);
	editorDrawMessageBar(&ab);
	
	char buf[32];
	snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1, 
											  (E.rx - E.coloff) + 1);
	abAppend(&ab, buf, strlen(buf));
	
	abAppend(&ab, "\x1b[?25h", 6);
	
	write(STDOUT_FILENO, ab.b, ab.len);
	abFree(&ab);
}

void editorSetStatusMessage(const char *fmt, ...) // Variadic function 
{												  // (any no. of args)
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
	va_end(ap);
	E.statusmsg_time = time(NULL);
}

/* Init */
void initEditor()
{
	E.cx = LINE_NUMBER_ENDPOS;
	E.cy = 0;
	E.rx = LINE_NUMBER_ENDPOS;
	E.rowoff = 0;
	E.coloff = 0;
	E.numrows = 0;
	E.row = NULL;
	E.dirty = 0;
	E.filename = NULL;
	E.statusmsg[0] = '\0';
	E.statusmsg_time = 0;
	E.syntax = NULL;
	
	if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
	E.screenrows -= 2;
}

int main(int argc, char *argv[])
{
	enableRawMode();
	initEditor();
	if (argc >= 2)
	{
		editorOpen(argv[1]);
	}
	
	editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = search | ARROW KEYS = scroll");
	
	while(1) // 'q' to quit program
	{
		editorRefreshScreen();
		editorProcessKeyPress();
	}
	return 0;
}
