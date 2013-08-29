/*
 * Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 *
 * File: domain-car-obs-torcs-types.h.
 * Main author: schwering.
 *
 * Low-level types for observations from TORCS for the continuous car domain.
 * It is for one about observations and for another about the processes which
 * represent a sample of the plan recognition system (probabilities are
 * sampled).
 */

#ifndef _OBS_H_
#define _OBS_H_

#include <assert.h>
#include <stdint.h>

#define PORT 19123

#define AGENTLEN  7

/* XXX Keep in sync with domain.car.agent_to_index! */
static inline int agent_to_index(const char *agent_name)
{
  if (!agent_name || !agent_name[0])
    return -1;
  return agent_name[0] - 'a';
}

struct agent_info_record {
  int8_t present;         /* zero iff no data present, otherwise non-zero */
  char agent[AGENTLEN+1]; /* name of agent */
  double veloc;           /* velocity of agent */
  double rad;             /* yaw of agent */
  double x;               /* longitudinal position of agent */
  double y;               /* lateral position of agent */
};

#define NAGENTS   10

/* Represents an observation of two agents and their physical parameters at a
 * certain point in time. */
struct observation_record {
  double t;         /* timestamp */
  uint8_t n_agents; /* number of agents in this observation */
  struct agent_info_record info[NAGENTS]; /* agent info */
};

#define NSOURCES 3

/* Global state of the plan recognition system. */
struct planrecog_state {
  uint8_t n_sources;
  struct {
    uint8_t working;  /* number of processes still working */
    uint8_t finished; /* number of succeeded processes */
    uint8_t failed;   /* number of failed processes */
  } sources[NSOURCES];
};

#endif

