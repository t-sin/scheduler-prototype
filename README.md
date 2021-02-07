# Scheduler prototype

A prototype of time-series event scheduling over threads.
It have two threads event scheduler and event processor.
The scheduler takes a predefined time-series events, then schedules events with elapsed time information.
If the events is fired, the scheduler notifies the events to the processor.

Here, the prototype will be realized as music seqnencer program.
In this case, the scheduler is an event sequencer and the processor is a synthesizer.
