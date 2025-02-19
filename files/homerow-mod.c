#include <linux/input.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>

#define LENGTH(A) (sizeof(A) / sizeof(A[0]))
#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#ifdef LOG
#define log eprintf
#else
#define log(...)
#endif

// Reuse a key not present on most keyboards for raise.
#define KEY_RAISE KEY_KATAKANAHIRAGANA
typedef struct input_event input_event;
typedef typeof(((input_event) {}).code) code;
typedef typeof(((input_event) {}).value) value;

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

#define BITSET(name, len) uint8_t name[((len) + 7) / 8]
bool bitset_get(uint8_t *bitset, int i) {
	return (bitset[i / 8] & (1 << (i % 8))) != 0;
}
void bitset_set(uint8_t *bitset, int i, bool value) {
	int index = i / 8, subindex = i % 8;
	uint8_t old_val = bitset[index];
	bool old = old_val & 1 << subindex;
	bitset[index] = old_val ^ ((old == 0) != (value == 0)) << subindex;
}
// Count of times each key appears in queue.
uint8_t key_queue_count[KEY_MAX];
// If the key was modified (turned to modifier) on it's last press.
BITSET(key_modified, KEY_MAX);
BITSET(key_raised, KEY_MAX);

input_event queue[1024];
bool queue_full = false;
int queue_front, queue_back;
bool queue_empty() { return queue_front == queue_back && !queue_full; }
input_event *queue_peek() { return &queue[queue_front]; }

enum key_code { UP = 0, DOWN = 1, REPEAT = 2 };

#ifdef DEBUG
input_event output[1024];
int output_head;
#endif

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
bool is_modifier(code code) { return translate_code(code) != code; }
bool modifier_active(code code) { return bitset_get(key_modified, code); }

void write_event(input_event *event, bool modify) {
	const code base_code = event->code;
	code translated = translate_code(event->code);
	if (modify && event->code != translated) {
		if (event->code == REPEAT)
			return;
		log("  modified\n");
		bitset_set(key_modified, base_code, event->value == DOWN);
		event->code = translated;
	}
#ifdef LOG
	if (event->type == EV_KEY)
		eprintf("-> "), print_key(event->code, event->value);
#endif
	static bool raise_active = false;
	if (event->code == KEY_RAISE) {
		raise_active = event->value != UP;
		goto end;
	}
	if (event->value == DOWN) {
		bitset_set(key_raised, base_code, raise_active);
	}
	if (event->value == DOWN ? raise_active
	                         : bitset_get(key_raised, base_code)) {
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
		case KEY_Z: event->code = KEY_102ND; break;
		case KEY_X: event->code = KEY_SYSRQ; break;
		case KEY_C: event->code = KEY_CAPSLOCK; break;
		case KEY_V: event->code = KEY_GRAVE; break;
		case KEY_B: event->code = KEY_COMPOSE; break;
		case KEY_N: event->code = KEY_BACKSLASH; break;
		case KEY_M: event->code = KEY_LEFTBRACE; break;
		/* case KEY_DOT: event->code = KEY_DOT; break; */
		/* case KEY_COMMA: event->code = KEY_COMMA; break; */
		case KEY_SLASH: event->code = KEY_RIGHTBRACE; break;
		case KEY_ENTER: event->code = KEY_DELETE; break;
		case KEY_BACKSPACE: event->code = KEY_MINUS; break;
		case KEY_APOSTROPHE: event->code = KEY_EQUAL; break;
		}
	}
#ifdef DEBUG
end:
	if (output_head >= LENGTH(output)) {
		eprintf("Bug: output too long.\n");
		exit(1);
	}
	output[output_head++] = *event;
#else
	fwrite(event, sizeof(*event), 1, stdout);
end:
#endif
	event->code = base_code;
}

// Advance the start of the list by one, removing the first element.
// Do not pop an empty queue.
void dequeue() {
	--key_queue_count[queue_peek()->code];
	queue_front = (queue_front + 1) % LENGTH(queue);
	queue_full = false;
}
void enqueue(input_event *event) {
	if (queue_full)
		return;
	queue[queue_back++] = *event;
	queue_back %= LENGTH(queue);
	++key_queue_count[event->code];
	if (queue_front == queue_back)
		queue_full = true;
}
bool in_queue(code code) { return key_queue_count[code] != 0; }

void handle_event(input_event *input) {
	if (input->type != EV_KEY) {
		write_event(input, false);
		return;
	}

	switch (input->code) {
	case KEY_CAPSLOCK: input->code = KEY_ESC; break;
	case KEY_LEFTSHIFT: input->code = KEY_CAPSLOCK; break;
	}

	if (queue_empty()) {
		if (input->value == DOWN && is_modifier(input->code)) {
			log("add to empty queue\n");
			enqueue(input);
		} else {
			log("write\n");
			write_event(input, modifier_active(input->code));
		}
	} else if (input->value == DOWN || !in_queue(input->code)) {
		log("add to queue\n");
		enqueue(input);
	} else {
		log("clear modify\n");
		for (; !queue_empty() && in_queue(input->code); dequeue()) {
			write_event(
				queue_peek(),
				(queue_peek()->value == DOWN
			     && queue_peek()->code != input->code)
					|| modifier_active(queue_peek()->code)
			);
		}
		enqueue(input);
		log("clear\n");
		for (; !queue_empty()
		       && (queue_peek()->value != DOWN
		           || !is_modifier(queue_peek()->code));
		     dequeue()) {
			write_event(queue_peek(), modifier_active(queue_peek()->code));
		}
	}
}

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
value key_status[KEY_MAX];
bool all_keys_up() {
	for (int i = 0; i < LENGTH(key_status); ++i)
		if (key_status[i] != UP)
			return false;
	return true;
}
const static code codes[]
	= { KEY_A, KEY_B, KEY_C, KEY_D, KEY_E, KEY_F, KEY_G, KEY_H, KEY_I,
	    KEY_J, KEY_K, KEY_L, KEY_M, KEY_N, KEY_O, KEY_P, KEY_Q, KEY_R,
	    KEY_S, KEY_T, KEY_U, KEY_V, KEY_W, KEY_X, KEY_Y, KEY_Z, KEY_SPACE };
bool finish_test() {
	eprintf("translated:\n");
	for (int i = 0; i < output_head; ++i) {
		eprintf(" ");
		print_key(output[i].code, output[i].value);
		key_status[output[i].code] = output[i].value;
	}
	if (!all_keys_up())
		return true;
	output_head = 0;
	eprintf("\n");
	return false;
}
int main(int argc, char **argv) {
	if (argc != 3)
		return 1;
	// Use od -vAn -td4 -N4 /dev/urandom for random seed.
	const int seed = atoi(argv[1]);
	const int fuzz_len = atoi(argv[2]);
	if (fuzz_len == 0)
		return 0;
	srandom(seed);
	static value press_state[LENGTH(codes)];
	// I was considering writing an algorithm to reduce the flagged results, but
	// you can just brute force it by reducing the FUZZ_LEN.
	// Technique is not very good because it usually selects 10 different keys.
	for (;;) {
		output_head = 0;
		for (int i = 0; i < fuzz_len; ++i) {
			int code = random() % LENGTH(codes);
			value value = press_state[code] == UP ? DOWN : UP;
			key(codes[code], value);
			press_state[code] = value;
		}
		for (int i = 0; i < LENGTH(codes); ++i)
			if (press_state[i] != UP)
				key(codes[i], UP), press_state[i] = UP;
		if (finish_test())
			break;
	}
	eprintf("\nfailed with seed %d, length %d\n", seed, fuzz_len);
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
