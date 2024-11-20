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
	struct node *prev, *next;
} node;
node key_status[KEY_MAX];
// Note: front_node may point to junk when back_node is NULL.
node *front_node, *back_node;

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
	fwrite(event, sizeof(*event), 1, stdout);
#endif
}
int keyboard_shift_held() {
	return key_status[KEY_LEFTSHIFT].event.value
	       || key_status[KEY_RIGHTSHIFT].event.value;
}

code translate_left(code code) {
	switch (code) {
	case KEY_F: return KEY_LEFTSHIFT;
	case KEY_D: return KEY_LEFTCTRL;
	case KEY_S: return KEY_LEFTMETA;
	case KEY_A: return KEY_LEFTALT;
	default: return code;
	}
}
code translate_right(code code) {
	switch (code) {
	case KEY_J: return KEY_RIGHTSHIFT;
	case KEY_K: return KEY_RIGHTCTRL;
	case KEY_L: return KEY_RIGHTMETA;
	// Avoid Alt Gr keys.
	case KEY_SEMICOLON: return KEY_LEFTALT;
	default: return code;
	}
}
bool left_code(code code) { return translate_left(code) != code; }
bool right_code(code code) { return translate_right(code) != code; }
// Use bitwise & to check any are non zero.
bool in_left() {
	return 0
	       != (key_status[KEY_F].event.value | key_status[KEY_D].event.value
	           | key_status[KEY_S].event.value | key_status[KEY_A].event.value);
}
bool in_right() {
	return 0
	       != (key_status[KEY_J].event.value | key_status[KEY_K].event.value
	           | key_status[KEY_L].event.value
	           | key_status[KEY_SEMICOLON].event.value);
}

// Advance the start of the list by one, removing the first element.
node *advance(node *n) {
	node *out = n->next;
	if (out != NULL)
		out->prev = NULL;
	n->next = NULL;
	return out;
}
bool in_queue(code code) {
	return key_status[code].next != NULL || key_status[code].prev != NULL
	       || back_node == &key_status[code];
}

enum mod_state { INACTIVE, QUEUEING, ACTIVE };
enum mod_side { LEFT, RIGHT };
typedef struct {
	enum mod_state state;
	enum mod_side side;
} mod;

code translate_generic(code code, enum mod_side side) {
	switch (side) {
	case LEFT: return translate_left(code);
	case RIGHT: return translate_right(code);
	}
	return code;
}

// Code should not already be in the queue.
void enqueue(code code) {
	node *this = &key_status[code];
	if (back_node == NULL) {
		back_node = this;
		front_node = this;
	} else {
		front_node->next = this;
		this->prev = front_node;
		front_node = this;
	}
}

void handle_event(input_event *input) {
	static bool disabled;
	static mod mod;

	if (input->type != EV_KEY) {
		write_event(input);
		return;
	}
	switch (input->code) {
	case KEY_CAPSLOCK:
		if (!keyboard_shift_held())
			input->code = KEY_ESC;
		break;
	case KEY_ESC:
		if (keyboard_shift_held() && input->value == 1) {
			disabled = !disabled;
			mod.state = INACTIVE;
			while (front_node != NULL)
				front_node = advance(front_node);
			return;
		}
		input->code = KEY_CAPSLOCK;
		break;
	}

	if (disabled) {
		key_status[input->code].event = *input;
		write_event(input);
		return;
	}
	switch (mod.state) {
	case INACTIVE:
		key_status[input->code].event = *input;
		if (input->value == 1 && left_code(input->code)) {
			mod.side = LEFT;
			goto start_queuing;
		}
		if (input->value == 1 && right_code(input->code)) {
			mod.side = RIGHT;
			goto start_queuing;
		}
		write_event(input);
		break;
	start_queuing:
		mod.state = QUEUEING;
		// TODO: add to queue
		enqueue(input->code);
		break;
	case QUEUEING:
		if (input->value == 1) {
			// Only an unmatched key up event can precede a down event in the
			// queue. Combine them into a 2.
			if (in_queue(input->code)) {
				input->value = 2;

				node *this = &key_status[input->code];
				if (this->next != NULL)
					this->next->prev = this->prev;
				if (this->prev != NULL)
					this->prev->next = this->next;
				if (back_node == this)
					back_node = this->next;
				if (front_node == this)
					front_node = this->prev;
			}
			enqueue(input->code);
		} else {
			if (in_queue(input->code)) {
				if (input->code == back_node->event.code)
					mod.state = INACTIVE;
				else
					mod.state = ACTIVE;
				node this
					= { .event = *input, .prev = front_node, .next = NULL };
				front_node->next = &this;
				front_node = &this;
				for (; back_node != NULL; back_node = advance(back_node)) {
					if (mod.state == ACTIVE) {
						back_node->event.code = translate_generic(
							back_node->event.code, mod.side
						);
					}
					write_event(&back_node->event);
				}
			} else {
				enqueue(input->code);
			}
		}
		key_status[input->code].event = *input;
		break;
	case ACTIVE:
		key_status[input->code].event = *input;
		input->code = translate_generic(input->code, mod.side);
		write_event(input);
		bool still_in_mod = mod.side == LEFT ? in_left() : in_right();
		if (!still_in_mod)
			mod.state = INACTIVE;
		break;
	}
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
