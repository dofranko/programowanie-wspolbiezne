with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ada.numerics.discrete_random;
with Ada.Numerics.Float_Random;

procedure Main is
    --basic constants
    n      : constant Integer := 10;
    d      : constant Integer := 20;
    k      : constant Integer := 7;
    minTime : constant Float := 0.8;
    maxTime : constant Float := 1.7;
    type NodesRange is new Integer range 0..n-1;
    Narray : array (NodesRange, NodesRange) of Boolean; --tablica krawędzi
    --Pakiety i wierzchołki
    type NodesRangeArray is array(NodesRange) of Boolean;
    type QueueArray is array(NodesRange range <>) of NodesRange; --tablica służąca za "wektor" dla połączeń danego wierzchołka
    type PacketRange is range 0..k-1; --zakres pakietów
    type Packet is record
        id : PacketRange;
        visitedNodes : NodesRangeArray := (NodesRange => False);
    end record;
    --randoms
    package Rand_Int is new ada.numerics.discrete_random(Integer);
    gen : Rand_Int.Generator;
    genF : Ada.Numerics.Float_Random.Generator;

    --funkcja losująca z przedziału czasu
    function RandFloat return Duration is 
    begin
        return Duration(minTime + Ada.Numerics.Float_Random.random(genF) * (maxTime-minTime));
    end RandFloat;
begin
    ----=======================
    --- Generowanie grafu i wyśiwetlenie
    ----=======================
    Rand_Int.reset(gen);
    Ada.Numerics.Float_Random.reset(genF);
    --generowanie przejść typu (i, i+1)
    Narray := (NodesRange => (NodesRange => False));
    for i in NodesRange loop
        if i < NodesRange'Last then
            Narray(i,i+1) := True;
        end if;
    end loop;

    --generowanie losowych skrótów
    declare
        a : NodesRange;
        b : NodesRange;
        i : Integer := 0;
    begin
        while i < d loop
            a := NodesRange(Rand_Int.random(gen) mod n);
            b := NodesRange(Rand_Int.random(gen) mod n);
            if not Narray(a,b) and a < b  then
                Narray(a,b) := True;
                i := i + 1;
            end if;
        end loop;
    end;

    --wypisanie grafu
    for i in NodesRange loop
        Put(NodesRange'Image(i));
        Put(": ");
        for ii in NodesRange loop
            if Narray(i, ii) then
                Put(NodesRange'Image(ii));
            end if;
        end loop;
        if i = NodesRange'Last then
            Put(Integer'Image(n));
        end if;
        Put_line("");
    end loop;


    ----=======================
    --- TASKI
    ----=======================
    declare 
        
        ---------task odbierający na końcu pakiety: odbiorca-----------
        task receiver is
            entry GetPacket(newPacket : Packet);
        end receiver;


        --------task wierzchołka grafu------------
        task type nodeWork is
            entry Start(yourId : NodesRange; isLast : Boolean);
            entry GetPacket(newPacket : Packet);
            entry Done;
            entry PrintRaport;
        end nodeWork;

        --deklaracja i uruchomienie TASKÓW WIERZCHOŁKÓW
        nodeTasks : array (NodesRange) of nodeWork;

        task body nodeWork is 
            id : NodesRange;
            receivedPackets :  array(PacketRange) of Integer := (PacketRange => -1);
            amILast : Boolean;
            edgesCount : NodesRange := 0; --liczba krawędzi "następnych" dla danego wierzchołka
        begin
            accept Start(yourId : NodesRange; isLast : Boolean) do
                id := yourId;
                amILast := isLast;
            end;
            
            --obliczenie liczby wierzchołków, do których można wysłać
            for i in NodesRange loop
                if Narray(id, i) then
                    edgesCount := edgesCount + 1;
                end if;
            end loop;

            declare
                myqueue : QueueArray(0..edgesCount); --"wektor" wierzchołków, do których można wysłać
                counter : NodesRange := 0;
                randNum : NodesRange;
                tmpPacket : packet;
                packetsCounter : Integer := 0;
            begin
                --stworzenie "wektora" następnych wierzchołków
                for i in NodesRange loop
                    if Narray(id, i) then
                        myqueue(counter) := i;
                        counter := counter + 1;
                    end if;
                end loop;
                loop
                    select
                        --pobranie pakietu i przetworzenie go
                        accept GetPacket(newPacket : Packet) do
                            tmpPacket := newPacket;
                        end GetPacket;
                        Put_line("pakiet" & PacketRange'Image(tmpPacket.id) & " jest w wierzchołku" & NodesRange'Image(id) );
                        delay RandFloat;
                        receivedPackets(tmpPacket.id) := packetsCounter;
                        tmpPacket.visitedNodes(id) := True;
                        --czy wysłać do losowego czy do receivera (odbiorcy)
                        if not amILast then
                            randNum := NodesRange(Rand_Int.random(gen) mod Integer(counter));
                            nodeTasks(myqueue(randNum)).GetPacket(tmpPacket);
                        else
                            receiver.GetPacket(tmpPacket);
                        end if;
                    or
                        --gdy wszystkie pakiety już doszły do receivera (odbiorcy)
                        accept Done;
                        accept PrintRaport do
                            --wydrukowanie otrzymanych pakietów dla danego wierzchołka
                            Put(NodesRange'Image(id));
                            Put(": ");
                            for i in PacketRange loop
                                for ii in PacketRange loop
                                    if receivedPackets(ii) = Integer(i) then
                                        Put(PacketRange'Image(ii));
                                    end if;
                                end loop;
                            end loop;
                            Put_line("");
                        end;
                        exit;
                    end select;
                end loop;
            end;
        end;

        ---------task odbierający na końcu pakiety: odbiorca----------------

        task body receiver is
            packets : array(PacketRange) of Packet;
            packetsCounter : Integer := 0;
        begin
            loop
                delay RandFloat;
                select
                    --odebranie pakietu i zapisanie do tablicy
                    accept GetPacket(newPacket : Packet) do
                        Put_line("pakiet" & PacketRange'Image(newPacket.id) & " został odebrany");
                        packets(newPacket.id) := newPacket; 
                    end GetPacket;
                    packetsCounter := packetsCounter + 1;
                or
                    --gdy odbierze wszystkie pakiety to wywołanie na każdym wierzchołku wypisania raportu
                    when packetsCounter = k => delay 0.0 ;
                    Put_line("Wierzchołki:");
                    for i in NodesRange loop
                        nodeTasks(i).Done;
                        nodeTasks(i).PrintRaport;
                    end loop;
                    --wypisanie raportów dla pakietów
                    Put_line("Pakiety ");
                    for i in packets'Range loop
                        Put(PacketRange'Image(packets(i).id));
                        Put(": ");
                        for ii in packets(i).visitedNodes'Range loop
                            if packets(i).visitedNodes(ii) then
                                Put(NodesRange'Image(ii));
                            end if;
                        end loop;
                        Put_line("");
                    end loop;
                    exit;
                end select;
            end loop;
        end;

        tmpPacket : packet;
    begin
        
        for i in NodesRange loop
            if i /= NodesRange'Last then
                nodeTasks(i).Start(i, False);
            else
                nodeTasks(i).Start(i, True);
            end if;
        end loop;
        for i in PacketRange loop
            delay RandFloat;
            tmpPacket := packet'(id => i, visitedNodes => (NodesRange => False));
            nodeTasks(0).GetPacket(tmpPacket);
        end loop;
    end;
    
end Main;
