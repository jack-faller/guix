#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define LENGTH(ARRAY) (sizeof(ARRAY) / sizeof(ARRAY[0]))

struct option options[] = { { "copy", required_argument, 0, 'c' },
	                        { "type", required_argument, 0, 't' },
	                        { "primary", no_argument, 0, 'p' },
	                        { "secondary", no_argument, 0, 's' },
	                        { "output", no_argument, 0, 'o' },
	                        { 0, 0, 0, 0 } };
char *opts = "t:c:pso";

int main(int argc, char **argv) {
	const char *op = "-in";
	const char *selection = "clipboard";
	const char *type = "text/plain";
	const char **text = calloc(argc, sizeof(char *));
	int text_len = 0;

	int ret;
	while (-1 != (ret = getopt_long(argc, argv, opts, options, NULL))) {
		switch (ret) {
		case 't': type = optarg; break;
		case 'c': text[text_len++] = optarg; break;
		case 'p': selection = "primary"; break;
		case 's': selection = "secondary"; break;
		case 'o': op = "-out"; break;
		default: return 1;
		}
	}

	const char *cmdline[]
		= { "xclip", op, "-selection", selection, "-target", type };
	int pipes[2];

	pipe(pipes);
	if (text_len != 0) {
		if (optind != argc) {
			fprintf(stderr, "Cannot do file input and --copy input\n");
			return 1;
		}
		if (fork() == 0) {
			close(pipes[0]);
			for (int i = 0; i < text_len; ++i)
				dprintf(pipes[1], "%s", text[i]);
			close(pipes[1]);
			return 1;
		}
		dup2(pipes[0], 0);
		close(pipes[0]);
		close(pipes[1]);
	}
	size_t final_len = LENGTH(cmdline) + argc - optind;
	char **cmdline_final = calloc(final_len, sizeof(char *));
	memcpy(cmdline_final, cmdline, sizeof(cmdline));
	memcpy(
		&cmdline_final[LENGTH(cmdline)],
		&argv[optind],
		sizeof(char *) * (argc - optind)
	);
	execvp("xclip", cmdline_final);
}
