/* vim: ft=c ts=4 sw=4 et wm=0 tw=0
 * Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 *
 * File: replay.c
 * Main author: schwering.
 *
 * Simulates a TORCS instance by emitting observations read from stdin or a
 * file. It cares about the time periods between observations.
 */

#include <netdb.h>
#include <netinet/in.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

#include "RSTC/obs.h"

//#define SERVER_MODE
#define HOST "localhost"
#define PORT 19123


#ifdef SERVER_MODE

static bool make_server_socket(int *sockfd)
{
	struct sockaddr_in server_addr;
	*sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (*sockfd < 0) {
		fprintf(stderr, "Couldn't open socket\n");
		exit(1);
	}
	bzero((char*) &server_addr, sizeof(server_addr));
	server_addr.sin_family = AF_INET;
	server_addr.sin_addr.s_addr = INADDR_ANY;
	server_addr.sin_port = htons(PORT);
	if (bind(*sockfd, (struct sockaddr*) &server_addr, sizeof(server_addr)) < 0) {
	fprintf(stderr, "Couldn't bind socket\n");
		exit(1);
	}
	listen(*sockfd, 1);
	return *sockfd != -1;
}

static bool accept_connection(int server_sockfd, int *sockfd)
{
	struct sockaddr_in client_addr;
	socklen_t client_len = sizeof(client_addr);
	*sockfd = accept(server_sockfd, (struct sockaddr*) &client_addr, &client_len);
	if (*sockfd < 0) {
		fprintf(stderr, "Couldn't accept connection\n");
		exit(1);
	}
	return *sockfd != -1;
}

#else

static bool make_socket(int *sockfd)
{
	struct sockaddr_in server_addr;
	struct hostent *server;

	*sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (*sockfd < 0) {
		fprintf(stderr, "Couldn't open socket\n");
		exit(1);
	}
	server = gethostbyname(HOST);
	if (server == NULL) {
		fprintf(stderr, "Couldn't resolve host %s\n", HOST);
		exit(1);
	}
	bzero((char *) &server_addr, sizeof(server_addr));
	server_addr.sin_family = AF_INET;
	bcopy(server->h_addr, &server_addr.sin_addr.s_addr, server->h_length);
	server_addr.sin_port = htons(PORT);
	if (connect(*sockfd, (struct sockaddr *) &server_addr, sizeof(server_addr)) < 0) {
		fprintf(stderr, "Couldn't connect to server\n");
		exit(1);
	}
	return *sockfd != -1;
}

#endif

static float min_conf(const struct planrecog_state *msg, const int i)
{
    const int numer = msg->sources[i].finished;
    const int denom = msg->sources[i].working + msg->sources[i].finished +
                      msg->sources[i].failed;
    return (double) numer / (double) denom;
}

static float max_conf(const struct planrecog_state *msg, const int i)
{
    const int numer = msg->sources[i].working + msg->sources[i].finished;
    const int denom = msg->sources[i].working + msg->sources[i].finished +
                      msg->sources[i].failed;
    return (double) numer / (double) denom;
}

static bool scan(FILE *fp, int n_agents, struct observation_record *r)
{
    int i;

    if (fscanf(fp, "%*c %lf", &r->t) != 1) {
        return false;
    }
    for (i = 0; i < n_agents; ++i) {
        int j;
        struct agent_info_record info;
        info.present = 1;
        if (fscanf(fp, "%s %lf %lf %lf %lf",
                    info.agent, &info.veloc,
                    &info.rad, &info.x, &info.y) != 5) {
            return false;
        }
        j = agent_to_index(info.agent);
        assert(0 <= j && j < NAGENTS);
        memcpy(&r->info[j], &info, sizeof(info));
    }
    if (fscanf(fp, "\n") != 0) {
        return false;
    }
    n_agents = i;
    r->n_agents = n_agents;
    return true;
}

static void klatschtgleich2(FILE *fp, int sockfd, int n_agents, bool do_sleep)
{
    struct observation_record r;
    struct planrecog_state state;
    double t0 = -1.0;

    memset(&r, 0, sizeof(r));

    while (scan(fp, n_agents, &r)) {
        int i, ret;

        printf("%lf", r.t);
        for (i = 0; i < NAGENTS; ++i) {
            if (r.info[i].present) {
                printf(" '%s' %lf %lf %lf %lf",
                       r.info[i].agent, r.info[i].veloc, r.info[i].rad,
                       r.info[i].x, r.info[i].y);
            }
        }
        printf("\n");

        if (do_sleep && t0 >= 0.0) {
            usleep((useconds_t) (1e6 * (r.t - t0)));
        }
        t0 = r.t;

        ret = write(sockfd, &r, sizeof(r));
        if (ret != sizeof(r)) {
            fprintf(stderr, "Couldn't write %lu bytes\n", sizeof(r));
            exit(1);
        }

        ret = read(sockfd, &state, sizeof(state));
        if (ret != sizeof(state)) {
            fprintf(stderr, "Couldn't read %lu bytes\n", sizeof(state));
            exit(1);
        }

        for (i = 0; i < NAGENTS; ++i) {
            int j;
            for (j = 0; j < NAGENTS; ++j) {
                if (r.info[i].present && r.info[j].present && i != j) {
                    const double ntg = (r.info[j].x - r.info[i].x) / r.info[i].veloc;
                    const double ttc = (r.info[j].x - r.info[i].x) / (r.info[i].veloc - r.info[j].veloc);
                    printf("ntg('%s', '%s') = %7.2lf\t\t", r.info[i].agent, r.info[j].agent, ntg);
                    printf("ttc('%s', '%s') = %7.2lf\n", r.info[i].agent, r.info[j].agent, ttc);
                }
            }
        }
        printf("%.2lf =< confidence working=%d, finished=%d, failed=%d =< %.2lf\n",
                min_conf(&state, 0), state.sources[0].working, state.sources[0].finished, state.sources[0].failed, max_conf(&state, 0));
    }
}

int main(int argc, char *argv[])
{
    int n_agents = 2;
    bool do_sleep = true;
#ifdef SERVER_MODE
    int server_sockfd;
#endif
    int sockfd;
    int flag_offset;
    int i;

    for (i = 1; i < argc; ++i) {
        const char *s = argv[i];
        int tmp;

        if (!strcmp(s, "-n") || !strcmp(s, "--no-sleep"))
            /* XXX Leads to crash for some reason (don't know why right now). */
            do_sleep = false;
        else if (sscanf(s, "-%d", &tmp) == 1)
            n_agents = tmp;
        else
            break;
    }
    flag_offset = i;

#ifdef SERVER_MODE
    if (!make_server_socket(&server_sockfd)) {
        fprintf(stderr, "Couldn't create server socket.\n");
        exit(1);
    }
    while (accept_connection(server_sockfd, &sockfd))
#else
    if (make_socket(&sockfd))
#endif
    {
        if (flag_offset == argc) {
            klatschtgleich2(stdin, sockfd, n_agents, do_sleep);
            fprintf(stderr, "Note: stdin doesn't work more than one iterations!\n");
        } else {
            for (i = flag_offset; i < argc; ++i) {
                FILE *fp = fopen(argv[i], "r");
                klatschtgleich2(fp, sockfd, n_agents, do_sleep);
                fclose(fp);
            }
        }
        close(sockfd);
    }
    return 0;
}

