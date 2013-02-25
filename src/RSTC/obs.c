#define _GNU_SOURCE
#include "obs.h"
#include <assert.h>
#include <math.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

#define MAX_OBSERVATIONS	1000

#ifndef NAN
#define NAN (0.0/0.0)
#endif

#define SERVER_MODE
#define HOST "localhost"
#define PORT 19123

static int server_sockfd = -1;
static int sockfd = -1;
static int last_obs = -1;
static struct observation_record obs[MAX_OBSERVATIONS];


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

static void finalize_connection(int *sockfd)
{
	close(*sockfd);
	*sockfd = -1;
}


static bool receive_obs(int sockfd, int obs_id)
{
	struct planrecog_state msg;
	int ret;

	ret = read(sockfd, &obs[obs_id], sizeof(obs[obs_id]));
	if (ret == sizeof(obs[obs_id])) {
		memset(&msg, 0, sizeof(msg));
		ret = write(sockfd, &msg, sizeof(msg));
		if (ret == sizeof(msg)) {
			return true;
		} else {
			goto error;
		}
	} else {
		goto error;
	}
error:
	close(sockfd);
	return false;
}


int obs_next(int obs_id)
{
	bool res = true;

#ifdef SERVER_MODE
	if (res && server_sockfd == -1) {
		res = make_server_socket(&server_sockfd);
	}
	if (res && sockfd == -1) {
		res = accept_connection(server_sockfd, &sockfd);
	}
#else
	if (res && sockfd == -1) {
		res = make_socket(&sockfd);
	}
#endif
	if (res && obs_id > last_obs) {
		int i;
		for (i = last_obs + 1; i <= obs_id; ++i) {
			res = receive_obs(sockfd, i);
		}
		last_obs = obs_id;
	}
	return (res) ? 1 : -1;
}


double obs_time(int obs_id)
{
	assert(0 <= obs_id && obs_id <= last_obs);

	return obs[obs_id].t;
}


int obs_lane(int obs_id, int B)
{
	struct agent_info_record *b;

	assert(0 <= obs_id && obs_id <= last_obs);
	assert(0 <= B && B < NAGENTS);

	b = &obs[obs_id].info[B];
	if (b->present) {
		if (b->y > 0.0) {
			return -1;
		} else {
			return 1;
		}
	}
	return 0;
}


double obs_ntg(int obs_id, int B, int C)
{
	struct agent_info_record *b;
	struct agent_info_record *c;

	assert(0 <= obs_id && obs_id <= last_obs);
	assert(0 <= B && B < NAGENTS);
	assert(0 <= C && C < NAGENTS);

	b = &obs[obs_id].info[B];
	c = &obs[obs_id].info[C];
	if (b->present && c->present) {
		const double v = b->veloc;
		if (v != 0.0) {
			return (c->x - b->x) / v;
		}
	}
	return NAN;
}


double obs_ttc(int obs_id, int B, int C)
{
	struct agent_info_record *b;
	struct agent_info_record *c;

	assert(0 <= obs_id && obs_id <= last_obs);
	assert(0 <= B && B < NAGENTS);
	assert(0 <= C && C < NAGENTS);

	b = &obs[obs_id].info[B];
	c = &obs[obs_id].info[C];
	if (b->present && c->present) {
		const double vd = b->veloc - c->veloc;
		if (vd != 0.0) {
			return (c->x - b->x) / vd;
		}
	}
	return NAN;
}

