#include <linux/input-event-codes.h>
#include <linux/input.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>

#define LENGTH(A) (sizeof(A) / sizeof(A[0]))
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

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

#ifdef DEBUG
input_event output[1024];
int output_head;
#endif

void write_event(input_event *event) {
#ifdef LOG
	void print_key(code code, value value);
	if (event->type == EV_KEY)
		eprintf("// "), print_key(event->code, event->value);
#endif
#ifdef DEBUG
	if (output_head >= LENGTH(output)) {
		eprintf("Bug: output too long.\n");
		exit(1);
	}
	if (event->type == EV_KEY)
		output[output_head++] = *event;
#endif
#ifndef DEBUG
	static value raise_active = UP;
	if (event->code == KEY_RAISE) {
		raise_active = event->value;
		return;
	}
	code old = event->code;
	if (raise_active != UP) {
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

	switch (input->code) {
	case KEY_LEFTSHIFT: input->code = KEY_CAPSLOCK; break;
	case KEY_CAPSLOCK: input->code = KEY_ESC; break;
	}

	const code input_code = input->code;
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
				input->code = key_status[input->code].event.code;
				write_event(input);
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
			while (oldest_node != NULL
			       && (in_queue(input->code) || oldest_node->event.value == UP
			           || !is_modifier(real_code(oldest_node)))) {
				if (oldest_node->event.value != UP
				    && real_code(oldest_node) != input->code)
					oldest_node->event.code
						= translate_code(real_code(oldest_node));
				write_event(&oldest_node->event);
				pop_queue();
			}
		}
		goto retry_event;
	} else {
		if (key_status[input->code].event.value != UP)
			input->code = key_status[input->code].event.code;
		enqueue(input_code);
	}

	key_status[input_code].event = *input;
}

#if defined(DEBUG) || defined(LOG)
const char *keyname(code code) {
	switch (code) {
	case KEY_A: return "KEY_A";
	case KEY_B: return "KEY_B";
	case KEY_C: return "KEY_C";
	case KEY_D: return "KEY_D";
	case KEY_E: return "KEY_E";
	case KEY_F: return "KEY_F";
	case KEY_G: return "KEY_G";
	case KEY_H: return "KEY_H";
	case KEY_I: return "KEY_I";
	case KEY_J: return "KEY_J";
	case KEY_K: return "KEY_K";
	case KEY_L: return "KEY_L";
	case KEY_M: return "KEY_M";
	case KEY_N: return "KEY_N";
	case KEY_O: return "KEY_O";
	case KEY_P: return "KEY_P";
	case KEY_Q: return "KEY_Q";
	case KEY_R: return "KEY_R";
	case KEY_S: return "KEY_S";
	case KEY_T: return "KEY_T";
	case KEY_U: return "KEY_U";
	case KEY_V: return "KEY_V";
	case KEY_W: return "KEY_W";
	case KEY_X: return "KEY_X";
	case KEY_Y: return "KEY_Y";
	case KEY_Z: return "KEY_Z";
	case KEY_SPACE: return "KEY_SPACE";
	case KEY_LEFTSHIFT: return "KEY_LEFTSHIFT";
	case KEY_LEFTCTRL: return "KEY_LEFTCTRL";
	case KEY_LEFTMETA: return "KEY_LEFTMETA";
	case KEY_LEFTALT: return "KEY_LEFTALT";
	case KEY_RIGHTSHIFT: return "KEY_RIGHTSHIFT";
	case KEY_RIGHTCTRL: return "KEY_RIGHTCTRL";
	case KEY_RIGHTMETA: return "KEY_RIGHTMETA";
	case KEY_RAISE: return "KEY_RAISE";
	default: return NULL;
	}
}
void print_key(code code, value value) {
	const char *name = keyname(code);
	if (name != NULL)
		eprintf("key(%s, %d);\n", name, (int)value);
	else
		eprintf("key(%d, %d);\n", (int)code, (int)value);
}
#endif
#ifdef DEBUG
void key(code code, value value) {
	input_event input
		= (input_event) { .code = code, .type = EV_KEY, .value = value };
	print_key(code, value);
	handle_event(&input);
}
void tap(code code) {
	key(code, 1);
	key(code, 0);
}
bool all_keys_up() {
	for (int i = 0; i < LENGTH(key_status); ++i)
		if (key_status[i].event.value != UP)
			return false;
	return true;
}
const static code codes[]
	= { KEY_A, KEY_B, KEY_C, KEY_D, KEY_E, KEY_F, KEY_G, KEY_H, KEY_I,
	    KEY_J, KEY_K, KEY_L, KEY_M, KEY_N, KEY_O, KEY_P, KEY_Q, KEY_R,
	    KEY_S, KEY_T, KEY_U, KEY_V, KEY_W, KEY_X, KEY_Y, KEY_Z, KEY_SPACE };
bool finish_test() {
	eprintf("translated:\n");
	for (int i = 0; i < output_head; ++i)
		eprintf(" "), print_key(output[i].code, output[i].value);
	if (!all_keys_up())
		return true;
	for (int i = 0; i < output_head; ++i)
		key_status[output[i].code].event = output[i];
	if (!all_keys_up())
		return true;
	output_head = 0;
	eprintf("\n");
	return false;
}
#define FINISH_TEST \
	if (finish_test()) { \
		eprintf("failed\n"); \
		return 1; \
	}
int main(int argc, char **argv) {
	key(KEY_J, 1);
	tap(KEY_K);
	tap(KEY_K);
	key(KEY_J, 0);
	FINISH_TEST;
	for (int i = 0; i < 2; ++i) {
		key(KEY_J, 1);
		key(KEY_T, 1);
		key(KEY_J, 0);
		key(KEY_T, 0);
	}
	FINISH_TEST;
	for (int i = 0; i < 2; ++i) {
		key(KEY_T, 1);
		key(KEY_J, 1);
		key(KEY_T, 0);
		key(KEY_J, 0);
	}
	FINISH_TEST;
	key(KEY_D, 1);
	key(KEY_J, 1);
	key(KEY_D, 0);
	key(KEY_J, 0);
	FINISH_TEST;
	for (int i = 0; i < 2; ++i) {
		key(KEY_RIGHTSHIFT, 1);
		tap(KEY_NUMLOCK);
		key(KEY_RIGHTSHIFT, 0);
	}
	key(KEY_J, 1);
	tap(KEY_D);
	key(KEY_J, 0);
	FINISH_TEST;
	key(KEY_H, 1);
	key(KEY_I, 1);
	key(KEY_S, 1);
	key(KEY_I, 0);
	key(KEY_H, 0);
	key(KEY_SPACE, 1);
	key(KEY_S, 0);
	key(KEY_SPACE, 0);
	FINISH_TEST;
	key(KEY_F, 1);
	key(KEY_A, 1);
	key(KEY_K, 1);
	key(KEY_A, 0);
	key(KEY_F, 0);
	key(KEY_K, 0);
	FINISH_TEST;
	key(KEY_S, 1);
	key(KEY_K, 1);
	key(KEY_S, 0);
	key(KEY_R, 1);
	key(KEY_S, 1);
	key(KEY_K, 0);
	key(KEY_R, 0);
	key(KEY_S, 0);
	FINISH_TEST;
	if (argc != 2)
		return 0;
	// Use od -vAn -td4 -N4 /dev/urandom for random seed.
	const int seed = atoi(argv[1]);
	srandom(seed);
	// I was considering writing an algorithm to reduce the flagged results, but
	// you can just brute force it by reducing the FUZZ_LEN.
#ifndef FUZZ_LEN
#define FUZZ_LEN 10
#endif
	// Technique is not very good because it usually selects 10 different keys.
	for (;;) {
		output_head = 0;
		for (int i = 0; i < FUZZ_LEN; ++i) {
			code code = codes[random() % LENGTH(codes)];
			key(code, key_status[code].event.value == UP ? DOWN : UP);
		}
		for (int i = 0; i < LENGTH(codes); ++i)
			if (key_status[codes[i]].event.value != UP)
				key(codes[i], UP);
		if (finish_test())
			break;
	}
	eprintf("\nseed %d\n", seed);
	bool code_set = false;
	static bool ignore_map[LENGTH(output)];
	return 1;
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
