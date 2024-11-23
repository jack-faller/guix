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
// Note: latest_node may point to junk when oldest_node is NULL.
node *latest_node, *oldest_node;

enum key_code { UP = 0, DOWN = 1, REPEAT = 2 };
enum mod_state { INACTIVE, QUEUEING, ACTIVE };
// TODO: maybe the algorithm shouldn't use groups.
enum mod_group { LEFT, RIGHT };
typedef struct {
	enum mod_state state;
	enum mod_group group;
} modifier_info;

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

#define LEFT_CASES(SHIFT, CTRL, META, ALT) \
	case KEY_F: SHIFT \
	case KEY_D: \
	CTRL case KEY_S: \
	META case KEY_A: \
		ALT
#define RIGHT_CASES(SHIFT, CTRL, META, ALT) \
	case KEY_J: SHIFT \
	case KEY_K: \
	CTRL case KEY_L: \
	META case KEY_SEMICOLON: \
		ALT

code translate_left(code code) {
	switch (code) {
		LEFT_CASES(return KEY_LEFTSHIFT;, return KEY_LEFTCTRL;
		           , return KEY_LEFTMETA;
		           , return KEY_LEFTALT;)
	default: return code;
	}
}
code translate_right(code code) {
	switch (code) {
		// Return left alt to avoid Alt Gr keys.
		RIGHT_CASES(return KEY_RIGHTSHIFT;, return KEY_RIGHTCTRL;
		            , return KEY_RIGHTMETA;
		            , return KEY_LEFTALT;)
	default: return code;
	}
}
// Use bitwise | to check any are non zero.
bool in_mod(enum mod_group group) {
	switch (group) {
	case LEFT:
		return 0
		       != (key_status[KEY_F].event.value | key_status[KEY_D].event.value
		           | key_status[KEY_S].event.value
		           | key_status[KEY_A].event.value);
	case RIGHT:
		return 0
		       != (key_status[KEY_J].event.value | key_status[KEY_K].event.value
		           | key_status[KEY_L].event.value
		           | key_status[KEY_SEMICOLON].event.value);
	}
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
	       || oldest_node == &key_status[code];
}

bool set_group(code code, enum mod_group *out) {
	switch (code) {
		LEFT_CASES(, , , )
		*out = LEFT;
		return true;
		RIGHT_CASES(, , , )
		*out = RIGHT;
		return true;
	default: return false;
	}
}
code translate_code(code code, enum mod_group side) {
	switch (side) {
	case LEFT: return translate_left(code);
	case RIGHT: return translate_right(code);
	default: return code;
	}
}

// Code should not already be in the queue.
void enqueue(code code) {
	node *this = &key_status[code];
	if (oldest_node == NULL) {
		oldest_node = this;
		latest_node = this;
	} else {
		latest_node->next = this;
		this->prev = latest_node;
		latest_node = this;
	}
}
void dequeue(code code) {
	node *this = &key_status[code];
	if (this->next != NULL)
		this->next->prev = this->prev;
	if (this->prev != NULL)
		this->prev->next = this->next;
	if (oldest_node == this)
		oldest_node = this->next;
	if (latest_node == this)
		latest_node = this->prev;
}

void handle_event(input_event *input) {
	static bool disabled;
	static modifier_info mod;

	if (input->type != EV_KEY) {
		write_event(input);
		return;
	}
	if (input->code == KEY_NUMLOCK
	    && key_status[KEY_RIGHTSHIFT].event.value != UP) {
		if (input->value == DOWN) {
			disabled = !disabled;
			mod.state = INACTIVE;
			while (oldest_node != NULL)
				oldest_node = advance(oldest_node);
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

	switch (mod.state) {
	case INACTIVE:
		key_status[input->code].event = *input;
		if (input->value == DOWN && set_group(input->code, &mod.group)) {
			mod.state = QUEUEING;
			enqueue(input->code);
		} else {
			write_event(input);
		}
		break;
	case QUEUEING:
		bool in_q = in_queue(input->code);
		if (input->value == DOWN || !in_q) {
			// Only an unmatched key up event can precede a down event in the
			// queue. Combine them into a repeat event. This slightly alters
			// order.
			if (in_q) {
				input->value = REPEAT;
				dequeue(input->code);
			}
			enqueue(input->code);
		} else {
			mod.state
				= input->code == oldest_node->event.code ? INACTIVE : ACTIVE;
			node this = { .event = *input, .prev = latest_node, .next = NULL };
			latest_node->next = &this;
			latest_node = &this;
			for (; oldest_node != NULL; oldest_node = advance(oldest_node)) {
				if (mod.state == ACTIVE)
					oldest_node->event.code
						= translate_code(oldest_node->event.code, mod.group);
				write_event(&oldest_node->event);
			}
		}
		key_status[input->code].event = *input;
		break;
	case ACTIVE:
		key_status[input->code].event = *input;
		input->code = translate_code(input->code, mod.group);
		write_event(input);
		if (!in_mod(mod.group))
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
