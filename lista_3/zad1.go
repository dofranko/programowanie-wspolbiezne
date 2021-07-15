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

type offer_packet struct {
	vertex_index int
	total_cost   int
}

type offer struct {
	pairs        []offer_packet
	sender_index int
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
	const n = 5
	const d = 3

	//losowość czasu
	var minTime = 0.8
	var maxTime = 2.3
	randDuration := func() time.Duration {
		return time.Duration(minTime + rand.Float64()*(maxTime-minTime))
	}

	var N [n][n]bool     //tablica krawędzi grafu
	var Nqueues [n][]int //tablica kolejek krawędzi (wygodniej operować losowością)
	var R [n][n]routing_cell

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
		fmt.Println(i, ":", routingrow)
	}

	// Stworzenie channeli między wierzchołkami(!)
	var vertex_channels = make([]chan offer, 0)
	for i := 0; i < n; i++ {
		vertex_channels = append(vertex_channels, make(chan offer))
	}

	/**
	===========================
	WĄTEK WIERZCHOŁKA
	===========================
	*/
	for vert_num := 0; vert_num < n; vert_num++ {

		go func(actual_index int) {
			var my_queue = Nqueues[actual_index]
			var my_vertex_num = actual_index
			var readChan = make(chan read_packet)   //channell do odczytu z Ri
			var writeChan = make(chan write_packet) //channel do zapisu w Ri
			//nadzorca nad Ri
			go func() {
				for {
					select {
					case readRequest := <-readChan:
						readRequest.return_chan <- R[my_vertex_num][readRequest.index]
					case writeRequest := <-writeChan:
						if writeRequest.do_change_only_changed {
							R[my_vertex_num][writeRequest.index].changed = writeRequest.value.changed
						} else {
							R[my_vertex_num][writeRequest.index] = writeRequest.value
						}
						writeRequest.return_chan <- true
					}
				}
			}()
			//Sender_i
			go func() {
				var my_return_read_chan = make(chan routing_cell)
				var my_return_write_chan = make(chan bool)
				for {
					time.Sleep(time.Second * randDuration())
					var newOffer = offer{sender_index: my_vertex_num, pairs: make([]offer_packet, 0)}
					for i := 0; i < n; i++ {
						if i == my_vertex_num {
							continue
						}
						readChan <- read_packet{index: i, return_chan: my_return_read_chan}
						var temp_cell = <-my_return_read_chan
						if temp_cell.changed {
							fmt.Println(my_vertex_num, " zmienia changed dla ", i, " na false")
							writeChan <- write_packet{
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
						for _, vertex_num := range my_queue {
							vertex_channels[vertex_num] <- newOffer
							fmt.Println(my_vertex_num, " wysyla oferte do ", vertex_num, "zawierającą: ", newOffer.pairs, " {do_wierzchołka, koszt}")
						}
					}
				}
			}()

			//Receiver_i
			go func() {
				var my_return_read_chan = make(chan routing_cell)
				var my_return_write_chan = make(chan bool)
				var my_vertex_channel = vertex_channels[my_vertex_num]
				for {
					temp_offer := <-my_vertex_channel
					fmt.Println(my_vertex_num, " otrzymuje oferte od ", temp_offer.sender_index, " zawierającą: ", temp_offer.pairs)
					for i := range temp_offer.pairs {
						var pair = temp_offer.pairs[i]
						var new_cost = pair.total_cost + 1
						readChan <- read_packet{index: pair.vertex_index, return_chan: my_return_read_chan}
						var temp_cell = <-my_return_read_chan
						if new_cost < temp_cell.cost {
							fmt.Println(my_vertex_num, " zmienia wpis dla ", pair.vertex_index, " na: ", routing_cell{cost: new_cost, nexthop: temp_offer.sender_index, changed: true})
							writeChan <- write_packet{
								index:                  pair.vertex_index,
								value:                  routing_cell{cost: new_cost, nexthop: temp_offer.sender_index, changed: true},
								do_change_only_changed: false,
								return_chan:            my_return_write_chan,
							}
							<-my_return_write_chan
						}
					}
				}
			}()

		}(vert_num)
	}

	time.Sleep(time.Second * 20)
	for i, routingrow := range R {
		fmt.Println(i, ":", routingrow)
	}

}
