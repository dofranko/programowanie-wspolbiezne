with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure zad1 is
    --basic constants
    n       : constant Integer := 9;
    d       : constant Integer := 4;
    h       : constant Integer := 4; --liczba hostow
    minTime : constant Float   := 2.8;
    maxTime : constant Float   := 4.3;

    type NodesRange is new Integer range 0 .. n - 1;
    type HostsRange is new Integer range 0 .. h - 1;
    Narray : array (NodesRange, NodesRange) of Boolean; --tablica krawędzi

    package NodesRange_Vector is new Ada.Containers.Vectors
       (Index_Type => Natural, Element_Type => NodesRange);

    Nqueue : array (NodesRange) of NodesRange_Vector.Vector;

    type NodesRangeArray is array (NodesRange) of Boolean;

    type Routing_cell is record
        next_hop : NodesRange;
        cost     : Integer;
        changed  : Boolean;
    end record;
    type Offer_packet is record
        vertex_index : NodesRange;
        total_cost   : Integer;
    end record;
    package Offer_packet_Vector is new Ada.Containers.Vectors
       (Index_Type => Natural, Element_Type => Offer_packet);

    type Offer is record
        pairs        : Offer_packet_Vector.Vector;
        sender_index : NodesRange;
    end record;

    type Address_pair is record
        router : NodesRange;
        host   : HostsRange;
    end record;

    type Host_info is record
        address    : Address_pair;
        host_index : HostsRange;
    end record;

    type Standard_packet is record
        sender          : Address_pair;
        receiver        : Address_pair;
        visited_routers : NodesRange_Vector.Vector;
    end record;
    package Standard_packet_Vector is new Ada.Containers.Vectors
       (Index_Type => Natural, Element_Type => Standard_packet);

    HostsArray : array (HostsRange) of Host_info;

    package Host_Vector is new Ada.Containers.Vectors
       (Index_Type => HostsRange, Element_Type => Host_info);

    ---------------- protected
    type RoutingRowArray is array (NodesRange) of Routing_cell;

    protected type routing_row is
        procedure Set (newCell : Routing_cell; idx : NodesRange);
        procedure SetChanged (newChanged : Boolean; idx : NodesRange);
        function Get return RoutingRowArray;
    private
        Local : RoutingRowArray;
    end routing_row;

    R_table : array (NodesRange) of routing_row;

    protected body routing_row is
        procedure Set (newCell : Routing_cell; idx : NodesRange) is
        begin
            Local (idx) := newCell;
        end Set;

        procedure SetChanged (newChanged : Boolean; idx : NodesRange) is
        begin
            Local (idx).changed := newChanged;
        end SetChanged;

        function Get return RoutingRowArray is
        begin
            return Local;
        end Get;
    end routing_row;

    protected type StandardPacketQueue is
        procedure RemoveFirst;
        procedure Enqueue (newElement : Standard_packet);
        function IsNotEmpty return Boolean;
        function GetFirst return Standard_packet;
    private
        MyStandards : Standard_packet_Vector.Vector := Standard_packet_Vector.Empty_Vector;
    end StandardPacketQueue;

    AllStandardPacketQueues : array(NodesRange) of StandardPacketQueue;

    protected body StandardPacketQueue is
        procedure RemoveFirst  is
        begin
            MyStandards.Delete_First ;
        end RemoveFirst;
        procedure Enqueue (newElement : Standard_packet) is
        begin
            MyStandards.Append (newElement);
        end Enqueue;

        function IsNotEmpty return Boolean is
        begin
            if MyStandards.Length > 0 then
                return True;
            end if;
            return False;
        end IsNotEmpty;

        function GetFirst return Standard_packet is
            tmpPacket : Standard_packet;
        begin
            tmpPacket := MyStandards.First_Element;
            return tmpPacket;
        end GetFirst;
    end StandardPacketQueue;

    --randoms
    package Rand_Int is new Ada.Numerics.Discrete_Random (Integer);
    gen  : Rand_Int.Generator;
    genF : Ada.Numerics.Float_Random.Generator;

    --funkcja losująca z przedziału czasu
    function RandFloat return Duration is
    begin
        return
           Duration
              (minTime +
               Ada.Numerics.Float_Random.Random (genF) * (maxTime - minTime));
    end RandFloat;
begin
    ----=======================
    --- Generowanie grafu i wyśiwetlenie
    ----=======================
    Rand_Int.Reset (gen);
    Ada.Numerics.Float_Random.Reset (genF);
    --generowanie przejść typu (i, i+1)
    Narray := (NodesRange => (NodesRange => False));
    for i in NodesRange loop
        if i < NodesRange'Last then
            Narray (i, i + 1) := True;
            Narray (i + 1, i) := True;
        end if;
    end loop;

    --generowanie losowych skrótów
    declare
        first_rand  : NodesRange;
        second_rand : NodesRange;
        i           : Integer := 0;
    begin
        while i < d loop
            first_rand  := NodesRange (Rand_Int.Random (gen) mod n);
            second_rand := NodesRange (Rand_Int.Random (gen) mod n);
            if not Narray (first_rand, second_rand) and
               first_rand < second_rand
            then
                Narray (first_rand, second_rand) := True;
                Narray (second_rand, first_rand) := True;
                i                                := i + 1;
            end if;
        end loop;
    end;

    --wypisanie grafu i zapełnienie tablicy vectorów zawierającej sąsiadów danego wierzchołka
    for i in NodesRange loop
        Put (NodesRange'Image (i));
        Put (": ");
        for ii in NodesRange loop
            if Narray (i, ii) then
                Nqueue (i).Append (ii);
                Put (NodesRange'Image (ii));
            end if;
        end loop;
        Put_Line ("");
    end loop;

    --zapełnienie tablicy R
    declare
        tmp : Routing_cell;
    begin
        for i in NodesRange loop --iteracja po R_table (wierszach)
            for ii in NodesRange loop --iteracja po komórkach
                if i = ii then
                    tmp :=
                       Routing_cell'
                          (next_hop => 0, cost => 0, changed => False);
                    R_table (i).Set (tmp, ii);
                elsif Nqueue (i).Contains (ii) then
                    tmp :=
                       Routing_cell'
                          (next_hop => ii, cost => 1, changed => True);
                    R_table (i).Set (tmp, ii);
                else
                    if i < ii then
                        tmp :=
                           Routing_cell'
                              (next_hop => i + 1, cost => Integer (ii - i),
                               changed  => True);
                    else
                        tmp :=
                           Routing_cell'
                              (next_hop => i - 1, cost => Integer (i - ii),
                               changed  => True);
                    end if;
                    R_table (i).Set (tmp, ii);
                end if;
            end loop;
        end loop;
        --wypisanie routing table
        Put_Line ("Wierzchołek: {routing_table: next_hop, cost, changed}");
        for i in NodesRange loop
            Put (NodesRange'Image (i));
            Put (": ");
            for ii in NodesRange loop
                tmp := R_table (i).Get (ii);
                Put ("{ ");
                Put (NodesRange'Image (tmp.next_hop));
                Put (Integer'Image (tmp.cost));
                Put (" ");
                Put (Boolean'Image (tmp.changed));
                Put ("}");
            end loop;
            Put_Line ("");
        end loop;
    end;

    --stworzenie hostów i ich wypisanie
    declare
        rand_router      : NodesRange;
        tmp_counter_host : HostsRange;
    begin
        Put_Line ("Lista hostów: (router, host)");
        for i in HostsRange loop
            tmp_counter_host := 0;
            rand_router      := NodesRange (Rand_Int.Random (gen) mod n);
            for ii in 0 .. i - 1 loop
                if HostsArray (ii).address.router = rand_router then
                    tmp_counter_host := tmp_counter_host + 1;
                end if;
            end loop;
            HostsArray (i) :=
               Host_info'
                  (address =>
                      Address_pair'
                         (router => rand_router, host => tmp_counter_host),
                   host_index => i);
            Put_Line
               ("(" & NodesRange'Image (rand_router) & ", " &
                HostsRange'Image (tmp_counter_host) & ")");
        end loop;
    end;

    declare

        task type receiver is
            entry Start (idx : NodesRange);
            entry SendOffer (newOffer : Offer);
        end receiver;

        task type sender is
            entry Start (idx : NodesRange);
        end sender;

        task type forwarder_receiver is
            entry Start (idx : NodesRange);
            entry SendStandardPacket(newPacket : Standard_packet);
        end forwarder_receiver;

        task type forwarder_sender is
            entry Start (idx : NodesRange);
        end forwarder_sender;

        task type host_node is
            entry Start(idx : HostsRange);
            entry SendStandardPacket(newPacket : Standard_packet);
        end host_node;

        receivers_tasks : array (NodesRange) of receiver;
        senders_tasks   : array (NodesRange) of sender;
        forwarder_receivers : array(NodesRange) of forwarder_receiver;
        forwarder_senders : array(NodesRange) of forwarder_sender;
        host_nodes : array(HostsRange) of host_node;
        
        ------- SENDER
        task body sender is
            tmpRow   : RoutingRowArray;
            tmpCell  : Routing_cell;
            newOffer : Offer;
            myindex  : NodesRange;
            msg      : Unbounded_String;
        begin
            accept Start (idx : NodesRange) do
                myindex := idx;
            end Start;
            loop
                delay RandFloat;
                tmpRow   := R_table (myindex).Get;
                newOffer :=
                   Offer'
                      (sender_index => myindex,
                       pairs        => Offer_packet_Vector.Empty_Vector);
                for i in NodesRange loop
                    if i /= myindex then
                        tmpCell := tmpRow (i);
                        if tmpCell.changed = True then
                            tmpCell.changed := False;
                            
                            R_table (myindex).SetChanged (False, i);
                            newOffer.pairs.Append
                               (Offer_packet'
                                   (vertex_index => i,
                                    total_cost   => tmpCell.cost));
                        end if;
                    end if;
                end loop;
                if newOffer.pairs.Length > 0 then
                    for receiver_index of Nqueue (myindex) loop
                        receivers_tasks (receiver_index).SendOffer (newOffer);
                        
                    end loop;
                end if;
            end loop;
        end sender;

        ------- RECEIVER
        task body receiver is
            myindex    : NodesRange;
            tmpRow     : RoutingRowArray;
            tmpOffer   : Offer;
            tmpNewCost : Integer;
            msg        : Unbounded_String;
        begin
            accept Start (idx : NodesRange) do
                myindex := idx;
            end Start;
            loop
                accept SendOffer (newOffer : Offer) do
                    tmpOffer := newOffer;
                end SendOffer;
                
                for pair of tmpOffer.pairs loop
                    tmpNewCost := pair.total_cost + 1;
                    tmpRow     := R_table (myindex).Get;
                    if tmpNewCost < tmpRow (pair.vertex_index).cost then
                        R_table (myindex).Set
                           (Routing_cell'
                               (next_hop => tmpOffer.sender_index,
                                cost     => tmpNewCost, changed => True),
                            pair.vertex_index);
                    tmpRow := R_table (myindex).Get;
                    msg := To_Unbounded_String ("");
                    msg := msg & NodesRange'Image(myindex) & ":";
                    for cell of tmpRow loop
                       msg := msg & "{" & NodesRange'Image(cell.next_hop) & " " & Integer'Image(cell.cost) & "} ";
                    end loop;
                    Put_Line (To_String(msg));
                    end if;
                end loop;
            end loop;
        end receiver;

        ----------
        --HOSTY I FORWARDERY
        ----------
        task body forwarder_receiver is
            myindex    : NodesRange;
            tmpPacket   : Standard_packet;
        begin
            accept Start(idx : NodesRange) do
               myindex := idx;
            end Start;
            loop
                accept SendStandardPacket(newPacket : Standard_packet) do
                    tmpPacket := newPacket;
                end SendStandardPacket;
                tmpPacket.visited_routers.Append (myindex);
                AllStandardPacketQueues(myindex).Enqueue (tmpPacket);
            end loop;
        end forwarder_receiver;

        task body forwarder_sender is
            myindex    : NodesRange;
            tmpPacket   : Standard_packet;
            my_hosts : Host_Vector.Vector;
        begin
            accept Start(idx : NodesRange) do
               myindex := idx;
            end Start;
            for i in HostsRange loop
                if HostsArray(i).address.router = myindex then
                    my_hosts.Append (HostsArray(i));
                end if;
            end loop;
            loop
               if AllStandardPacketQueues(myindex).IsNotEmpty then
                    tmpPacket := AllStandardPacketQueues(myindex).GetFirst;
                    if tmpPacket.receiver.router = myindex then
                        host_nodes(my_hosts(tmpPacket.receiver.host).host_index).SendStandardPacket (tmpPacket);
                    else
                        forwarder_receivers(R_table(myindex).Get(tmpPacket.receiver.router).next_hop)
                        .SendStandardPacket (tmpPacket);
                    end if;
                    AllStandardPacketQueues(myindex).RemoveFirst;
               end if;
               delay Duration(RandFloat / n);
            end loop;
            
        end forwarder_sender;

        task body host_node is
            myindex    : HostsRange;
            tmpPacket   : Standard_packet;
            my_info : Host_info;
            tmp_host_num : HostsRange;
            msg      : Unbounded_String;
        begin
            accept Start(idx : HostsRange) do
                myindex := idx;
            end Start;
            my_info := HostsArray(myindex);
            tmp_host_num := HostsRange(Rand_Int.Random(gen) mod h);
            while tmp_host_num = myindex loop
               tmp_host_num := HostsRange(Rand_Int.Random(gen) mod h);
            end loop;
            
            tmpPacket := Standard_packet'(
                sender => my_info.address, 
                receiver => HostsArray(tmp_host_num).address, 
                visited_routers => NodesRange_Vector.Empty_Vector
                );
            forwarder_receivers(my_info.address.router).SendStandardPacket (tmpPacket);
            loop
               accept SendStandardPacket(newPacket : Standard_packet) do
                  tmpPacket := newPacket;
               end SendStandardPacket;
               msg := To_Unbounded_String ("");
               msg := msg & "[" & NodesRange'Image(tmpPacket.sender.router) & " " & HostsRange'Image(tmpPacket.sender.host) & "] ";
               msg := msg & "[" & NodesRange'Image(tmpPacket.receiver.router) & " " & HostsRange'Image(tmpPacket.receiver.host) & "] ";
               msg := msg & "(";
               for nod of tmpPacket.visited_routers loop
                  msg := msg & NodesRange'Image(nod) & " ";
               end loop;
               msg := msg & ")";
               Put_Line (To_String(msg));
               tmpPacket := Standard_packet'(
                   sender => my_info.address, 
                   receiver => tmpPacket.sender, 
                   visited_routers => NodesRange_Vector.Empty_Vector
                   );
                forwarder_receivers(my_info.address.router).SendStandardPacket (tmpPacket);
                delay Duration(RandFloat / (n-1));
            end loop;
        end host_node;
    begin
        for i in NodesRange loop
            receivers_tasks (i).Start (i);
            senders_tasks (i).Start (i);
            forwarder_receivers(i).Start (i);
            forwarder_senders(i).Start (i);
        end loop;
        for i in HostsRange loop
           host_nodes(i).Start (i);
        end loop;
        delay 20.0;

    end;
end zad1;
