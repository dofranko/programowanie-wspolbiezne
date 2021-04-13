package main

import (
	"fmt"
	"math/rand"
	"sort"
	"strings"
	"time"
)

//globals
var minTime, maxTime float64

type packet struct {
	id           int
	visitedNodes []int
}

///gorutine drukarki wiadomości
func printer(msgChan <-chan string, doneChanPrinter chan bool, n int) {
	for {
		select {
		case msg := <-msgChan:
			fmt.Println(msg)
		case <-doneChanPrinter:
			//gdy etap końcowy, to drukarka zbiera raporty od wierchołków i je sortuje
			receivedReports := make([]string, 0)
			for i := 0; i < n; i++ {
				tmp := <-msgChan
				receivedReports = append(receivedReports, tmp)
			}
			sort.Strings(receivedReports)
			for i := 0; i < n; i++ {
				fmt.Println(receivedReports[i])
			}
			doneChanPrinter <- true //potwierdzenie skończenia drukarki
			return
		}
	}
}

//gorutine wierzchołków
func nodeWork(id int, channels []chan *packet, myqueue []int, msgChan chan<- string, doneChan <-chan bool) {
	mychannel := channels[id]       //id wierzchołka
	receivedNodes := make([]int, 0) //tablica id otrzymanych pakietów
	for {
		select {
		case newPacket := <-mychannel:
			//pobranie i przetworzenie pakietu
			fmt.Println("pakiet", newPacket.id, "jest w wierzchołku", id)
			receivedNodes = append(receivedNodes, newPacket.id)
			newPacket.visitedNodes = append(newPacket.visitedNodes, id)
			time.Sleep(time.Second * randDuration())
			channels[myqueue[rand.Intn(len(myqueue))]] <- newPacket
		case <-doneChan:
			msgChan <- fmt.Sprintf("%d: %s", id, strings.Trim(strings.Join(strings.Fields(fmt.Sprint(receivedNodes)), " "), "[]"))
			return
		}
	}
}

func randDuration() time.Duration {
	return time.Duration(minTime + rand.Float64()*(maxTime-minTime))
}

func main() {
	//inicjalizacja losowowości losowania
	rand.Seed(time.Now().UnixNano())
	//stałe
	const n = 10
	const d = 20
	const k = 7

	//losowość czasu
	minTime = 0.8
	maxTime = 2.3

	var N [n][n]bool     //tablica krawędzi grafu
	var Nqueues [n][]int //tablica kolejek krawędzi (wygodniej operować losowością)

	//po ukośniej usunięcie połączeń oraz połączenie (i, i+1)
	for i := 0; i < n; i++ {
		N[i][i] = false
		if i+1 < n {
			N[i][i+1] = true
		}
	}
	//losowe d połączeń
	for i := 0; i < d; i++ {
		a, b := rand.Intn(n), rand.Intn(n)
		if !N[a][b] && a < b {
			N[a][b] = true
		} else {
			i--
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
	Nqueues[len(Nqueues)-1] = append(Nqueues[len(Nqueues)-1], n)
	for i, queue := range Nqueues {
		fmt.Println(i, ": ", queue)
	}

	///Rozpoczęcie pogramu na channelach

	var msgChan = make(chan string, n)     //kanał do drukowania
	var doneChan = make(chan bool)         //kanał do kończenia pracy (tylko wierzchołki)
	var doneChanPrinter = make(chan bool)  //kanał do finalnej części drukarki (raportów)
	var outChan = make(chan *packet)       //kanał do odbiorcy (ostatni)
	var channels = make([]chan *packet, 0) //tablica kanałów do wierzchołków
	var packets [k]packet                  //tablica pakietów

	//stworzenie tablicy kanałów wierzchołków
	for i := 0; i < n; i++ {
		channels = append(channels, make(chan *packet, n))
	}
	//dodanie odbiorcy na koniec tablicy kanałów
	channels = append(channels, outChan)

	//rozpoczęcie pracy drukarki
	go printer(msgChan, doneChanPrinter, n)

	//rozpoczęcie pracy wierzchołków
	for i := 0; i < n; i++ {
		go nodeWork(i, channels, Nqueues[i], msgChan, doneChan)
	}

	//stworznie i rozpoczęcie pracy nadawcy (tworzenie i wysyłanie k nowych pakietów)
	sender := func() {
		for i := 0; i < k; i++ {
			time.Sleep(time.Second * randDuration())
			newPacket := packet{id: i, visitedNodes: make([]int, 0)}
			channels[0] <- &newPacket
		}
	}
	go sender()

	//Odbiorca - odbieranie pakietów
	for i := 0; i < k; i++ {
		time.Sleep(time.Second * randDuration())
		receivedPacket := <-outChan
		packets[receivedPacket.id] = *receivedPacket
		msgChan <- fmt.Sprintf("pakiet %d został odebrany", receivedPacket.id)
	}
	time.Sleep(time.Second * randDuration())

	//przesłanie do wierzchołków informacji o końcu pakietów
	for i := 0; i < n; i++ {
		doneChan <- true
	}
	msgChan <- "Wierzchołki grafu:"
	//przesłanie do drukarki informacji o końcu pakietów
	doneChanPrinter <- true

	//oczekiwanie na koniec pracy drukarki
	<-doneChanPrinter

	fmt.Println("Pakiety: ")

	//wypisanie informacji pakietów
	for i := 0; i < k; i++ {
		fmt.Println(i, ": ", packets[i].visitedNodes)
	}

}
