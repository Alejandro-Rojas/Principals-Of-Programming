/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Description: Prg04 - Publish Subscribe Simulation
 * Student(s) Name(s): Alejandro Rojas
 */

package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

type PubSub struct {
	mu     sync.Mutex
	topics map[string][]chan string
	closed bool
}

func NewPubSub() *PubSub {
	ps := &PubSub{
		mu:     sync.Mutex{},
		topics: map[string][]chan string{},
		closed: false,
	}
	ps.topics = make(map[string][]chan string)
	return ps
}

var wg sync.WaitGroup

// TODO: create and return a new channel on a given topic, updating the PubSub struct; this is a helper method used by subscriber goroutines
func (ps *PubSub) subscribe(topic string) chan string {
	ps.mu.Lock()
	defer ps.mu.Unlock()
	ch := make(chan string, 1)
	ps.topics[topic] = append(ps.topics[topic], ch)
	return ch
}

// TODO: write the given message on all the channels associated with the given topic; if the topic does not exist, do nothing; this is a helper method used by publisher goroutines
func (ps *PubSub) publish(topic string, msg string) {
	ps.mu.Lock()
	defer ps.mu.Unlock()
	if ps.closed {
		return
	}
	for _, ch := range ps.topics[topic] {
		go func(ch chan string) {
			ps.newMethod(ch, msg)
		}(ch)
	}
}

func (*PubSub) newMethod(ch chan string, msg string) {
	ch <- msg
}

// TODO: send messages taken from a given array of message, one at a time and at random intervals, to all topic subscribers; this is the code template used by publisher goroutines
func publisher(ps *PubSub, topic string, msgs []string) {
	for _, msg := range msgs {
		time.Sleep(time.Duration(rand.Intn(20)) * time.Second)
		ps.publish(topic, msg)
	}
	wg.Done()
}

// TODO: read and display all messages received from a particular topic; this is the code template used by subscriber goroutines
func subscriber(ps *PubSub, name string, topic string) {
	ch := ps.subscribe(topic)
	for {
		if msg, ok := <-ch; ok {
			fmt.Println(name + " Received: " + msg)
		} else {

			break

		}
	}
}

func main() {
	// TODO: create the ps struct
	ps := NewPubSub()
	// TODO: create the arrays of messages to be sent on each topic
	BeesMessages := []string{
		"bees are pollinators",
		"bees produce honey,",
		"all worker bees are female,",
		"bees have 5 eyes",
		"bees fly about 20mph."}

	ColoradoMessages := []string{
		"Colfax in Denver is the longest continuous street in America,",
		"Colorado has the highest mean altitude of all the states,",
		"Colorado state flower is the columbine."}

	// TODO: set wait group to 2 (# of publishers)
	wg.Add(2)
	// TODO: create the publisher goroutines
	go publisher(ps, "Bees", BeesMessages)
	go publisher(ps, "Colorado", ColoradoMessages)
	// TODO: create the subscriber goroutines
	go subscriber(ps, "Publisher", "Colorado")
	go subscriber(ps, "Publisher", "Bees")
	go subscriber(ps, "John", "Bees")
	go subscriber(ps, "Mary", "Bees")
	go subscriber(ps, "Mary", "Colorado")
	// John is only interested in "bees"

	// Mary is interested in "bees" and "colorado"

	// TODO: wait for all publishers to be done
	wg.Wait()
	fmt.Println("publisher is done.")
	fmt.Println("publisher is done.")
}
