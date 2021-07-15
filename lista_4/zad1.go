package main

import (
	"fmt"
	"math"
	"math/rand"
	"time"
)

type routing_cell struct {
	nexthop int
	cost    int
	changed bool
}

type read_packet struct {
	index       int
	return_chan chan routing_cell
}

type write_packet struct {
	index                  int
	value                  routing_cell
	do_change_only_changed bool
	return_chan            chan bool
}

type read_queue struct {
	return_chan chan []standard_packet
}

type enqueue_queue struct {
	value       standard_packet
	return_chan chan bool
}

type dequeue_queue struct {
	return_chan chan bool
}

type offer_packet struct {
	vertex_index int
	total_cost   int
}

type offer struct {
	pairs        []offer_packet
	sender_index int
}

type address_pair struct {
	router int
	host   int
}

type host_info struct {
	address address_pair
	channel chan standard_packet
}

type standard_packet struct {
	sender          address_pair
	receiver        address_pair
	visited_routers []int
}

func contains(arr []int, num int) bool {
	for _, v := range arr {
		if v == num {
			return true
		}
	}
	return false
}

func main() {
	//inicjalizacja losowowości losowania
	rand.Seed(time.Now().UnixNano())
	//stałe
	const n = 9
	const d = 4
	const h = 4 //number of hosts
	//losowość czasu
	var minTime = 2.8
	var maxTime = 4.3
	randDurationSeconds := func() time.Duration {
		return time.Duration(minTime+rand.Float64()*(maxTime-minTime)) * time.Second
	}
	var printlnChan = make(chan string)
	go func() {
		for {
			message := <-printlnChan
			fmt.Println(message)
		}
	}()

	var N [n][n]bool               //tablica krawędzi grafu
	var Nqueues [n][]int           //tablica kolejek krawędzi (wygodniej operować losowością)
	var R [n][n]routing_cell       //tablica routingu
	var Hosts [h]host_info         //tablica hostów (na potrzeby losowania)
	var RouterHosts [n][]host_info //przypisania routerów i ich hostó
	//po ukośniej usunięcie połączeń oraz połączenie (i, i+1)
	for i := 0; i < n; i++ {
		N[i][i] = false
		if i+1 < n {
			N[i][i+1] = true
			N[i+1][i] = true
		}
	}
	//losowe d połączeń
	for i := 0; i < d; i++ {
		first_rand, second_rand := rand.Intn(n), rand.Intn(n)
		if !N[first_rand][second_rand] && first_rand < second_rand {
			N[first_rand][second_rand] = true
		} else {
			i--
		}
	}

	for i := 0; i < n; i++ {
		for ii := 0; ii < n; ii++ {
			if N[i][ii] {
				N[ii][i] = true
			}
		}
	}

	//wypisanie grafu
	for i := 0; i < n; i++ {
		Nqueues[i] = make([]int, 0)
		for ii := 0; ii < n; ii++ {
			if N[i][ii] {
				//zapełnienie kolejek krawędzi wierzchołków
				Nqueues[i] = append(Nqueues[i], ii)
			}
		}
	}
	fmt.Println("Wierzchołek: [..krawędzie]")
	for i, queue := range Nqueues {
		fmt.Println(i, ": ", queue)
	}

	//initialize routing table
	for i, routingrow := range R {
		for ii := range routingrow {
			if i == ii {
				continue
			}
			if contains(Nqueues[i], ii) {
				//gdy sąsiedzi
				R[i][ii].cost = 1
				R[i][ii].nexthop = ii
				R[i][ii].changed = true
			} else {
				//gdy nie sąsiedzi
				if i < ii {
					R[i][ii].nexthop = i + 1
				} else {
					R[i][ii].nexthop = i - 1
				}
				R[i][ii].cost = int(math.Abs(float64(i - ii)))
				R[i][ii].changed = true
			}
		}
	}
	fmt.Println("Wierzchołek: [routing_table: {next_hop, cost, changed}]")
	for i, routingrow := range R {
		fmt.Println(i, " : ", routingrow)
	}
	printRoutingTable := func(index int) {
		printlnChan <- fmt.Sprintf("%d: %v", index, R[index])
	}
	time.Sleep(time.Millisecond * 400)
	//stworzenie par hostów
	for i := 0; i < n; i++ {
		RouterHosts[i] = make([]host_info, 0)
	}
	fmt.Println("Lista hostów: (router, host)")
	for i := 0; i < h; i++ {
		router_num := rand.Intn(n)
		host_num := len(RouterHosts[router_num])
		new_address_pair := address_pair{host: host_num, router: router_num}
		new_host_info := host_info{address: new_address_pair, channel: make(chan standard_packet)}
		RouterHosts[router_num] = append(RouterHosts[router_num], new_host_info)
		Hosts[i] = new_host_info
		fmt.Println(new_address_pair)
	}

	/**
	---------------------
	Channele
	----------------------
	*/

	//Stworzenie channeli między wierzchołkami
	var vertex_channels = make([]chan offer, 0)
	for i := 0; i < n; i++ {
		vertex_channels = append(vertex_channels, make(chan offer))
	}
	//Stworzenie channeli dla forwarderów
	var forwarder_channels = make([]chan standard_packet, 0)
	for i := 0; i < n; i++ {
		forwarder_channels = append(forwarder_channels, make(chan standard_packet))
	}

	/**
	===========================
	WĄTEK WIERZCHOŁKA
	===========================
	*/
	for vert_num := 0; vert_num < n; vert_num++ {

		go func(actual_index int) {
			var my_neighbours_queue = Nqueues[actual_index]
			var my_router_num = actual_index
			var readRoutingTableChan = make(chan read_packet)   //channell do odczytu z Ri
			var writeRoutingTableChan = make(chan write_packet) //channel do zapisu w Ri
			//nadzorca nad Ri
			go func() {
				for {
					select {
					case readRequest := <-readRoutingTableChan:
						readRequest.return_chan <- R[my_router_num][readRequest.index]
					case writeRequest := <-writeRoutingTableChan:
						if writeRequest.do_change_only_changed {
							R[my_router_num][writeRequest.index].changed = writeRequest.value.changed
						} else {
							R[my_router_num][writeRequest.index] = writeRequest.value
						}
						writeRequest.return_chan <- true
					}
				}
			}()
			//Sender_i
			go func() {
				time.Sleep(time.Second * 4)
				var my_return_read_chan = make(chan routing_cell)
				var my_return_write_chan = make(chan bool)
				for {
					time.Sleep(randDurationSeconds())
					var newOffer = offer{sender_index: my_router_num, pairs: make([]offer_packet, 0)}
					for i := 0; i < n; i++ {
						if i == my_router_num {
							continue
						}
						readRoutingTableChan <- read_packet{index: i, return_chan: my_return_read_chan}
						var temp_cell = <-my_return_read_chan
						if temp_cell.changed {
							writeRoutingTableChan <- write_packet{
								index:                  i,
								value:                  routing_cell{nexthop: temp_cell.nexthop, cost: temp_cell.cost, changed: false},
								do_change_only_changed: true,
								return_chan:            my_return_write_chan,
							}
							<-my_return_write_chan
							newOffer.pairs = append(newOffer.pairs, offer_packet{vertex_index: i, total_cost: temp_cell.cost})
						}
					}
					if len(newOffer.pairs) > 0 {
						for _, vertex_num := range my_neighbours_queue {
							vertex_channels[vertex_num] <- newOffer
						}
					}
				}
			}()

			//Receiver_i
			go func() {
				time.Sleep(time.Second * 4)
				var my_return_read_chan = make(chan routing_cell)
				var my_return_write_chan = make(chan bool)
				var my_vertex_channel = vertex_channels[my_router_num]
				for {
					temp_offer := <-my_vertex_channel
					for i := range temp_offer.pairs {
						var pair = temp_offer.pairs[i]
						var new_cost = pair.total_cost + 1
						readRoutingTableChan <- read_packet{index: pair.vertex_index, return_chan: my_return_read_chan}
						var temp_cell = <-my_return_read_chan
						if new_cost < temp_cell.cost {
							writeRoutingTableChan <- write_packet{
								index:                  pair.vertex_index,
								value:                  routing_cell{cost: new_cost, nexthop: temp_offer.sender_index, changed: true},
								do_change_only_changed: false,
								return_chan:            my_return_write_chan,
							}
							<-my_return_write_chan
							printRoutingTable(my_router_num)
						}
					}
				}
			}()

			//Forwarder_i
			var my_forwarder_channel = forwarder_channels[my_router_num]
			var readQueueChanForwarder = make(chan read_queue)
			var enqueueChanForwarder = make(chan enqueue_queue)
			var dequeueChanForwarder = make(chan dequeue_queue)
			var my_hosts_pairs = RouterHosts[actual_index]
			//nadzorca nad forwarderem
			go func() {
				var standard_packets_queue = make([]standard_packet, 0)
				for {
					select {
					case readRequest := <-readQueueChanForwarder:
						readRequest.return_chan <- standard_packets_queue
					case enqueueRequest := <-enqueueChanForwarder:
						standard_packets_queue = append(standard_packets_queue, enqueueRequest.value)
						enqueueRequest.return_chan <- true
					case dequeueRequest := <-dequeueChanForwarder:
						standard_packets_queue = standard_packets_queue[1:]
						dequeueRequest.return_chan <- true
					}
				}
			}()
			//Forwarder_receiver
			go func() {
				var my_return_chan = make(chan bool)
				for {
					tmp_standard_packet := <-my_forwarder_channel
					tmp_standard_packet.visited_routers = append(tmp_standard_packet.visited_routers, my_router_num)
					enqueueChanForwarder <- enqueue_queue{value: tmp_standard_packet, return_chan: my_return_chan}
					<-my_return_chan
				}
			}()
			//Forwarder_sender
			go func() {
				var my_return_chan_queue_standard_packets = make(chan []standard_packet)
				var my_return_chan_dequeue = make(chan bool)
				var my_return_chan_routing_cell = make(chan routing_cell)
				for {
					readQueueChanForwarder <- read_queue{return_chan: my_return_chan_queue_standard_packets}
					tmp_queue := <-my_return_chan_queue_standard_packets
					if len(tmp_queue) > 0 {
						tmp_standard_packet := tmp_queue[0]
						if tmp_standard_packet.receiver.router == my_router_num {
							host_receiver := my_hosts_pairs[tmp_standard_packet.receiver.host]
							host_receiver.channel <- tmp_standard_packet
						} else {
							readRoutingTableChan <- read_packet{
								index:       tmp_standard_packet.receiver.router,
								return_chan: my_return_chan_routing_cell,
							}
							tmp_routing_cell_receiver := <-my_return_chan_routing_cell
							forwarder_channels[tmp_routing_cell_receiver.nexthop] <- tmp_standard_packet
						}
						dequeueChanForwarder <- dequeue_queue{return_chan: my_return_chan_dequeue}
						<-my_return_chan_dequeue
					}
					time.Sleep(randDurationSeconds() / n)
				}
			}()

		}(vert_num)
	}

	//host goroutines
	for h_num := 0; h_num < h; h_num++ {
		go func(actual_index int) {
			var my_host_channel = Hosts[actual_index].channel
			var my_address = Hosts[actual_index].address
			var my_router_forwarder_channel = forwarder_channels[my_address.router]

			first_tmp_host_num := rand.Intn(h)
			for first_tmp_host_num == actual_index {
				first_tmp_host_num = rand.Intn(h)
			}
			first_tmp_host_address := Hosts[first_tmp_host_num].address
			first_tmp_standard_packet := standard_packet{
				sender:          my_address,
				receiver:        first_tmp_host_address,
				visited_routers: make([]int, 0),
			}
			my_router_forwarder_channel <- first_tmp_standard_packet

			for {
				tmp_standard_packet := <-my_host_channel
				printlnChan <- fmt.Sprintf("%v", tmp_standard_packet)
				new_standard_packet := standard_packet{
					sender:          my_address,
					receiver:        tmp_standard_packet.sender,
					visited_routers: make([]int, 0),
				}
				my_router_forwarder_channel <- new_standard_packet
				time.Sleep(randDurationSeconds() / (n - 1))
			}
		}(h_num)
	}
	time.Sleep(time.Second * 30)

}
