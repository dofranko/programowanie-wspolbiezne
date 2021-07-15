with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ada.numerics.discrete_random;
with Ada.Numerics.Float_Random;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
procedure zad1 is
    --basic constants
    n      : constant Integer := 10;
    d      : constant Integer := 20;
    k      : constant Integer := 7;
    --l2: nowe stałe
    b : constant Integer := 6;
    h : constant Integer := 10;
    minTime : constant Float := 0.8;
    maxTime : constant Float := 1.7;
    type NodesRange is new Integer range 0..n-1;
    Narray : array (NodesRange, NodesRange) of Boolean; --tablica krawędzi
    --wektory
    package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Integer);
    use Integer_Vectors;

    --Pakiety i wierzchołki
    type NodesRangeArray is array(NodesRange) of Boolean;
    type QueueArray is array(NodesRange range <>) of NodesRange; --tablica służąca za "wektor" dla połączeń danego wierzchołka
    type PacketRange is range 0..k-1; --zakres pakietów
    type Packet is record
        id : PacketRange;
        visitedNodes : Vector;
        life : Integer := h; --l2: pozostałe życie pakietu
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
        first_rand : NodesRange;
        second_rand : NodesRange;
        i : Integer := 0;
    begin
        while i < d loop
            first_rand := NodesRange(Rand_Int.random(gen) mod n);
            second_rand := NodesRange(Rand_Int.random(gen) mod n);
            if not Narray(first_rand,second_rand) and first_rand < second_rand  then
                Narray(first_rand,second_rand) := True;
                i := i + 1;
            end if;
        end loop;
    end;
    --l2: generowanie b połączeń
    declare
        first_rand : NodesRange;
        second_rand : NodesRange;
        i : Integer := 0;
    begin
        while i < b loop
            first_rand := NodesRange(Rand_Int.random(gen) mod n);
            second_rand := NodesRange(Rand_Int.random(gen) mod n);
            if not Narray(first_rand,second_rand) and first_rand > second_rand  then
                Narray(first_rand,second_rand) := True;
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
            receivedPackets :  Vector;
            amILast : Boolean;
        begin
            accept Start(yourId : NodesRange; isLast : Boolean) do
                id := yourId;
                amILast := isLast;
            end;
            

            declare
                myqueue : Vector; --"wektor" wierzchołków, do których można wysłać
                randNum : Integer;
                tmpPacket : packet;
            begin
                --stworzenie "wektora" następnych wierzchołków
                for i in NodesRange loop
                    if Narray(id, i) then
                        myqueue.Append(Integer(i));
                    end if;
                end loop;
                if amILast then
                    myqueue.Append(-1);
                end if;
                loop
                    select
                        --pobranie pakietu i przetworzenie go
                        accept GetPacket(newPacket : Packet) do
                            tmpPacket := newPacket;
                        end GetPacket;
                        Put_line("pakiet" & PacketRange'Image(tmpPacket.id) & " jest w wierzchołku" & NodesRange'Image(id) );
                        receivedPackets.Append(Integer(tmpPacket.id));
                        --l2: przetworzneie życia pakietu
                        tmpPacket.life := tmpPacket.life - 1;
                        tmpPacket.visitedNodes.Append(Integer(id));
                        if tmpPacket.life = 0 then
                            --wysłanie paietu do śmietnika jeśli umarł
                            receiver.GetPacket(tmpPacket);
                        else
                            delay RandFloat;

                            Sending_loop: loop
                                --czy wysłać do losowego czy do receivera (odbiorcy)
                                randNum := (Rand_Int.random(gen) mod Integer(myqueue.Length));
                                if myqueue.Element(randNum) = -1 then
                                    receiver.GetPacket(tmpPacket);
                                    exit Sending_loop;
                                else
                                    select
                                        nodeTasks(NodesRange(myqueue.Element(randNum))).GetPacket(tmpPacket);
                                        exit Sending_loop;
                                    or 
                                        delay Duration(maxTime * 5.0);
                                        Put_line("(" & NodesRange'Image(id) & "=>" & Integer'Image(myqueue.Element(randNum)) &  ")" &"time-out: ponawianie próby");
                                        tmpPacket.life := tmpPacket.life - 1;
                                        if tmpPacket.life = 0 then
                                            receiver.GetPacket(tmpPacket);
                                            exit Sending_loop;
                                        end if;
                                    end select;
                                end if;
                            end loop Sending_loop;
                        end if;
                    or
                        --gdy wszystkie pakiety już doszły do receivera (odbiorcy)
                        accept Done;
                        accept PrintRaport do
                            --wydrukowanie otrzymanych pakietów dla danego wierzchołka
                            Put(NodesRange'Image(id));
                            Put(": ");
                            for packetId of receivedPackets loop
                               Put(Integer'Image(packetId));
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
            tmpPacket : Packet;
        begin
            loop
                delay RandFloat;
                select
                    --odebranie pakietu i zapisanie do tablicy
                    accept GetPacket(newPacket : Packet) do
                        tmpPacket := newPacket;
                    end GetPacket;
                    if tmpPacket.life > 0 then
                        Put_line("pakiet" & PacketRange'Image(tmpPacket.id) & " został odebrany");
                    else
                        Put_line("pakiet" & PacketRange'Image(tmpPacket.id) & " umarł");
                    end if;
                    packets(tmpPacket.id) := tmpPacket; 
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
                        for nodeId of packets(i).visitedNodes loop
                            Put(Integer'Image(nodeId));
                        end loop;
                        Put(" (");
                        Put(Integer'Image(packets(i).life));
                        Put(" )");
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
            tmpPacket := packet'(id => i, visitedNodes=>Empty_Vector, life => h);
            nodeTasks(0).GetPacket(tmpPacket);
        end loop;
    end;
    
end zad1;
