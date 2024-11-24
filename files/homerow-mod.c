#include <linux/input-event-codes.h>
#include <linux/input.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>

#define LENGTH(A) (sizeof(A) / sizeof(A[0]))

typedef struct input_event input_event;
typedef typeof(((input_event) {}).code) code;
typedef typeof(((input_event) {}).value) value;

typedef struct node {
	input_event event;
	struct node *next;
} node;
// Reuse a key not present on most keyboards for raise.
#define KEY_RAISE KEY_KATAKANAHIRAGANA
node key_status[KEY_MAX];
code real_code(node *n) { return n - key_status; }
// Note: latest_node may point to junk when oldest_node is NULL.
node *latest_node, *oldest_node;
bool queue_empty() { return oldest_node == NULL; }

enum key_code { UP = 0, DOWN = 1, REPEAT = 2 };

void write_event(input_event *event) {
#if defined(DEBUG) || defined(LOG)
	if (event->type == EV_KEY)
		fprintf(
			stderr,
			"{.code = %d, .value = %d}\n",
			(int)event->code,
			(int)event->value
		);
#endif
#ifndef DEBUG
	static value raise_active = UP;
	if (event->code == KEY_RAISE)
		raise_active = event->value;
	code old = event->code;
	if (raise_active == UP) {
		switch (event->code) {
		case KEY_LEFTSHIFT: event->code = KEY_CAPSLOCK; break;
		case KEY_CAPSLOCK: event->code = KEY_ESC; break;
		}
	} else {
		switch (event->code) {
		case KEY_Q: event->code = KEY_F1; break;
		case KEY_W: event->code = KEY_F2; break;
		case KEY_E: event->code = KEY_F3; break;
		case KEY_R: event->code = KEY_F4; break;
		case KEY_T: event->code = KEY_F5; break;
		case KEY_Y: event->code = KEY_F6; break;
		case KEY_U: event->code = KEY_F7; break;
		case KEY_I: event->code = KEY_F8; break;
		case KEY_O: event->code = KEY_F9; break;
		case KEY_P: event->code = KEY_F10; break;
		case KEY_A: event->code = KEY_1; break;
		case KEY_S: event->code = KEY_2; break;
		case KEY_D: event->code = KEY_3; break;
		case KEY_F: event->code = KEY_4; break;
		case KEY_G: event->code = KEY_5; break;
		case KEY_H: event->code = KEY_6; break;
		case KEY_J: event->code = KEY_7; break;
		case KEY_K: event->code = KEY_8; break;
		case KEY_L: event->code = KEY_9; break;
    case KEY_SEMICOLON: event->code = KEY_0; break;
		}
	}
	fwrite(event, sizeof(*event), 1, stdout);
	event->code = old;
#endif
}

code translate_code(code code) {
	switch (code) {
	case KEY_F: return KEY_LEFTSHIFT;
	case KEY_D: return KEY_LEFTCTRL;
	case KEY_S: return KEY_LEFTMETA;
	case KEY_A: return KEY_LEFTALT;
	case KEY_J: return KEY_RIGHTSHIFT;
	case KEY_K: return KEY_RIGHTCTRL;
	case KEY_L: return KEY_RIGHTMETA;
	case KEY_SEMICOLON: return KEY_LEFTALT; // Use left alt to avoid AltGr.
	case KEY_SPACE: return KEY_RAISE;
	default: return code;
	}
}
bool is_modifier(code code) { return code != translate_code(code); }
bool modifier_active(code code) { return code == key_status[code].event.value; }

// Advance the start of the list by one, removing the first element.
// Do not pop an empty queue.
void pop_queue() {
	node *n = oldest_node;
	oldest_node = oldest_node->next;
	n->next = NULL;
}
bool in_queue(code code) {
	return key_status[code].next != NULL || latest_node == &key_status[code];
}

// Code should not already be in the queue.
void enqueue(code code) {
	node *this = &key_status[code];
	if (oldest_node == NULL) {
		oldest_node = this;
		latest_node = this;
	} else {
		latest_node->next = this;
		latest_node = this;
	}
}
void dequeue(code code) {
	node *this = &key_status[code];
	if (oldest_node == this)
		oldest_node = this->next;
	this->next = NULL;
}

void handle_event(input_event *input) {
	static bool disabled;

	if (input->type != EV_KEY) {
		write_event(input);
		return;
	}
	if (input->code == KEY_NUMLOCK
	    && key_status[KEY_RIGHTSHIFT].event.value != UP) {
		if (input->value == DOWN) {
			disabled = !disabled;
			while (oldest_node != NULL)
				pop_queue();
		}
		return;
	}

	if (disabled) {
		key_status[input->code].event = *input;
		write_event(input);
		return;
	}

retry_event:
	if (queue_empty()) {
		if (is_modifier(input->code)) {
			switch (input->value) {
			case DOWN: enqueue(input->code); break;
			case REPEAT:
				if (!modifier_active(input->code)) {
					write_event(input);
				}
				break;
			case UP: {
				input_event modified = *input;
				modified.code = key_status[input->code].event.code;
				write_event(&modified);
			} break;
			}
		} else {
			write_event(input);
		}
	} else if (in_queue(input->code)) {
		if (input->value == DOWN) {
			dequeue(input->code);
			input->value = REPEAT;
		} else {
			for (; oldest_node != NULL
			       && (in_queue(input->code) || oldest_node->event.value == UP
			           || !is_modifier(real_code(oldest_node)));
			     pop_queue()) {
				if (oldest_node->event.value != UP
				    && real_code(oldest_node) != input->code)
					oldest_node->event.code
						= translate_code(real_code(oldest_node));
				write_event(&oldest_node->event);
			}
		}
		goto retry_event;
	} else {
		enqueue(input->code);
	}

	key_status[input->code].event = *input;
}

#ifdef DEBUG
void key(code code, value value) {
	input_event input
		= (input_event) { .code = code, .type = EV_KEY, .value = value };
	handle_event(&input);
}
void tap(code code) {
	key(code, 1);
	key(code, 0);
}
int main(void) {
	key(KEY_J, 1);
	tap(KEY_K);
	tap(KEY_K);
	key(KEY_J, 0);
	printf("\n");
	for (int i = 0; i < 2; ++i) {
		key(KEY_J, 1);
		key(KEY_T, 1);
		key(KEY_J, 0);
		key(KEY_T, 0);
	}
	printf("\n");
	for (int i = 0; i < 2; ++i) {
		key(KEY_T, 1);
		key(KEY_J, 1);
		key(KEY_T, 0);
		key(KEY_J, 0);
	}
	printf("\n");
	key(KEY_D, 1);
	key(KEY_J, 1);
	key(KEY_D, 0);
	key(KEY_J, 0);
	printf("\n");
	for (int i = 0; i < 2; ++i) {
		key(KEY_RIGHTSHIFT, 1);
		tap(KEY_NUMLOCK);
		key(KEY_RIGHTSHIFT, 0);
	}
	key(KEY_J, 1);
	tap(KEY_D);
	key(KEY_J, 0);
}
#else
int main(int argc, char **argv) {
	setbuf(stdin, NULL), setbuf(stdout, NULL);
	input_event input;
	while (!feof(stdin)) {
		if (1 != fread(&input, sizeof(input), 1, stdin))
			continue;
		handle_event(&input);
	}
}
#endif
