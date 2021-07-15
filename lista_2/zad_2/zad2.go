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
	life         int //l2: pozostały czas życia pakietu
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
func nodeWork(id int, channels []chan *packet, myqueue []int, msgChan chan<- string, doneChan <-chan bool, trashChan chan<- *packet, hunterChannels []chan bool) {
	mychannel := channels[id]             //id wierzchołka
	myhunterchannel := hunterChannels[id] //l2: kanał kłusownika
	receivedNodes := make([]int, 0)       //tablica id otrzymanych pakietów
	isThereATrap := false                 //L2: flaga pułapki
	for {
		select {
		case <-myhunterchannel:
			//l2: jeśli kłusownik tu przyjdzie
			isThereATrap = true
		case newPacket := <-mychannel:
			//pobranie i przetworzenie pakietu
			fmt.Println("pakiet", newPacket.id, "jest w wierzchołku", id)
			receivedNodes = append(receivedNodes, newPacket.id)
			newPacket.visitedNodes = append(newPacket.visitedNodes, id)
			if isThereATrap {
				//l2: jeśli jest pułapka to złapanie pakietu
				isThereATrap = false
				newPacket.life = -2
				trashChan <- newPacket
				break
			}
			//l2: odejmowanie życia pakietu, jeśli umiera to wysłanie do "kosza"
			newPacket.life--
			if newPacket.life <= 0 {
				trashChan <- newPacket
				break
			}
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
	//l2: nowe stałe
	const b = 5
	const h = 7

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
		first_rand, second_rand := rand.Intn(n), rand.Intn(n)
		if !N[first_rand][second_rand] && first_rand < second_rand {
			N[first_rand][second_rand] = true
		} else {
			i--
		}
	}

	//l2:
	//losowe b połączeń
	for i := 0; i < b; i++ {
		first_rand, second_rand := rand.Intn(n), rand.Intn(n)
		if !N[first_rand][second_rand] && first_rand > second_rand {
			N[first_rand][second_rand] = true
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

	var msgChan = make(chan string, n)    //kanał do drukowania
	var doneChan = make(chan bool)        //kanał do kończenia pracy (tylko wierzchołki)
	var doneChanPrinter = make(chan bool) //kanał do finalnej części drukarki (raportów)
	//l2
	var trashChan = make(chan *packet)        //kanał do usuwania straconych pakietów
	var doneChanHunter = make(chan bool)      //kanał końca pracy kłusownika
	var hunterChannels = make([]chan bool, 0) //tablica kanałów do wierzchołków
	var outChan = make(chan *packet)          //kanał do odbiorcy (ostatni)
	var channels = make([]chan *packet, 0)    //tablica kanałów do wierzchołków
	var packets [k]packet                     //tablica pakietów

	//stworzenie tablicy kanałów wierzchołków i kłusowników
	for i := 0; i < n; i++ {
		channels = append(channels, make(chan *packet, n))
		hunterChannels = append(hunterChannels, make(chan bool, n))
	}
	//dodanie odbiorcy na koniec tablicy kanałów
	channels = append(channels, outChan)

	//rozpoczęcie pracy drukarki
	go printer(msgChan, doneChanPrinter, n)

	//rozpoczęcie pracy wierzchołków
	for i := 0; i < n; i++ {
		go nodeWork(i, channels, Nqueues[i], msgChan, doneChan, trashChan, hunterChannels)
	}

	//stworznie i rozpoczęcie pracy nadawcy (tworzenie i wysyłanie k nowych pakietów)
	sender := func() {
		for i := 0; i < k; i++ {
			time.Sleep(time.Second * randDuration())
			newPacket := packet{id: i, visitedNodes: make([]int, 0), life: h} //--zad2 modified
			channels[0] <- &newPacket
		}
	}
	go sender()

	//l2:
	//stworznie i rozpoczęcie pracy nadawcy (tworzenie i wysyłanie k nowych pakietów)
	hunter := func() {
		for {
			select {
			case <-doneChanHunter:
				return
			case <-time.After(time.Second * time.Duration(maxTime) * randDuration() * 2):
				hunterChannels[rand.Intn(n)] <- true
			}
		}
	}
	go hunter()

	//Odbiorca - odbieranie pakietów
	for i := 0; i < k; i++ {
		time.Sleep(time.Second * randDuration())
		select {
		case receivedPacket := <-outChan:
			packets[receivedPacket.id] = *receivedPacket
			msgChan <- fmt.Sprintf("pakiet %d został odebrany", receivedPacket.id)
		//l2: odebranie pakietu z kosza
		case deadPacket := <-trashChan:
			packets[deadPacket.id] = *deadPacket
			if deadPacket.life == -2 {
				msgChan <- fmt.Sprintf("pakiet %d został złowiony", deadPacket.id)
			} else {
				msgChan <- fmt.Sprintf("pakiet %d umarł", deadPacket.id)
			}
		}
	}
	time.Sleep(time.Second * randDuration())

	doneChanHunter <- true //zakończenie pracy kłusownika
	//przesłanie do wierzchołków informacji o końcu pakietów
	for i := 0; i < n; i++ {
		doneChan <- true
	}
	msgChan <- "Wierzchołki grafu:"
	//przesłanie do drukarki informacji o końcu pakietów
	doneChanPrinter <- true

	//oczekiwanie na koniec pracy drukarki
	<-doneChanPrinter

	fmt.Println("Pakiety(nr: [odwiedzone wierzchołki] (pozostały czas życia)): ")

	//wypisanie informacji pakietów
	for i := 0; i < k; i++ {
		if packets[i].life == -2 {
			fmt.Println(i, ": ", packets[i].visitedNodes, "(złowiony)")
		} else if packets[i].life > 0 {
			fmt.Println(i, ": ", packets[i].visitedNodes, "(", packets[i].life, ")")
		} else {
			fmt.Println(i, ": ", packets[i].visitedNodes, "(umarł)")
		}
	}

}
