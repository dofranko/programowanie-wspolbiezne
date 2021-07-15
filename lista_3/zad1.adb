with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ada.numerics.discrete_random;
with Ada.Numerics.Float_Random;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;


procedure zad1 is
    --basic constants
    n      : constant Integer := 5;
    d      : constant Integer := 3;

    minTime : constant Float := 0.8;
    maxTime : constant Float := 1.7;

    type NodesRange is new Integer range 0..n-1;
    Narray : array (NodesRange, NodesRange) of Boolean; --tablica krawędzi
    
    package NodesRange_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => NodesRange);

    Nqueue : array(NodesRange) of NodesRange_Vector.Vector;

    type NodesRangeArray is array(NodesRange) of Boolean;

    type Routing_cell is record
        next_hop : NodesRange;
        cost : Integer;
        changed : Boolean; 
    end record;
    type Offer_packet is record
        vertex_index : NodesRange;
        total_cost : Integer;
    end record;
    package Offer_packet_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Offer_packet);
    
    type Offer is record
        pairs : Offer_packet_Vector.Vector;
        sender_index : NodesRange;
    end record;

    ---------------- protected
    type RoutingRowArray is array(NodesRange) of Routing_cell;
    protected type routing_row is
        procedure Set (newCell : Routing_cell; idx : NodesRange);
        procedure SetChanged(newChanged : Boolean; idx : NodesRange);
        function Get return RoutingRowArray;
    private
        Local : RoutingRowArray;
    end routing_row;

    R_table : array(NodesRange) of routing_row;

    protected body routing_row is
       procedure Set (newCell : Routing_cell; idx : NodesRange) is
       begin
            Local(idx) := newCell;
       end Set;

       procedure SetChanged(newChanged : Boolean; idx : NodesRange) is
       begin
            Local(idx).changed := newChanged;
       end SetChanged;

       function Get return RoutingRowArray is
       begin
            return Local;
       end Get;
    end routing_row;

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
            Narray(i+1,i) := True;
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
                Narray(second_rand,first_rand) := True;
                i := i + 1;
            end if;
        end loop;
    end;
    

    --wypisanie grafu i zapełnienie tablicy vectoró zawierającej sąsiadów danego wierzchołka
    for i in NodesRange loop
        Put(NodesRange'Image(i));
        Put(": ");
        for ii in NodesRange loop
            if Narray(i, ii) then
                Nqueue(i).Append(ii);
                Put(NodesRange'Image(ii));
            end if;
        end loop;
        Put_line("");
    end loop;

    --zapełnienie tablicy R
    declare
        tmp : Routing_cell;
    begin
        for i in NodesRange loop --iteracja po R_table (wierszach)
            for ii in NodesRange loop --iteracja po komórkach
                if i = ii then
                    tmp := Routing_cell'(next_hop => 0, cost => 0, changed => False);
                    R_table(i).Set(tmp, ii);
                elsif Nqueue(i).Contains(ii) then
                    tmp := Routing_cell'(next_hop => ii, cost => 1, changed => True);
                    R_table(i).Set(tmp, ii);
                else
                    if i < ii then
                        tmp := Routing_cell'(next_hop => i+1, cost => Integer(ii-i), changed => True);
                    else
                        tmp := Routing_cell'(next_hop => i-1, cost => Integer(i-ii), changed => True);
                    end if;
                    R_table(i).Set(tmp, ii);
                end if;
            end loop;
        end loop;
        --wypisanie routing table
        Put_line("Wierzchołek: {routing_table: next_hop, cost, changed}");
        for i in NodesRange loop
            Put(NodesRange'Image(i));
            Put(": ");
            for ii in NodesRange loop
                tmp := R_table(i).Get(ii);
                Put("{ ");
                Put(NodesRange'Image(tmp.next_hop));
                Put(Integer'Image(tmp.cost));
                Put(" ");
                Put(Boolean'Image(tmp.changed));
                Put("}");
            end loop;
            Put_line("");
        end loop;
    end;

    declare
       
    
        task type receiver is
            entry Start(idx : NodesRange);
            entry SendOffer(newOffer : Offer);
        end receiver;

        task type sender is
            entry Start(idx : NodesRange);
        end sender;

        receivers_tasks : array(NodesRange) of receiver;
        senders_tasks : array(NodesRange) of sender;
        
        ------- SENDER
        task body sender is
            tmpRow : RoutingRowArray;
            tmpCell : Routing_cell;    
            newOffer : Offer;
            myindex : NodesRange;
            msg : Unbounded_String;
        begin
            accept Start(idx : NodesRange) do
                myindex := idx;
            end Start;
            loop
                delay RandFloat;
                tmpRow := R_table(myindex).Get;
                newOffer := Offer'(sender_index => myindex, pairs => Offer_packet_Vector.Empty_Vector);
                for i in NodesRange loop
                   if i /= myindex then
                        tmpCell := tmpRow(i);
                        if tmpCell.changed = True then
                            tmpCell.changed := False;
                            Put_line(NodesRange'Image(myindex) & " zmienia changed dla " & NodesRange'Image(i) & " false");
                            R_table(myindex).SetChanged(False, i);
                            newOffer.pairs.Append(Offer_packet'(vertex_index => i, total_cost => tmpCell.cost));
                        end if;
                   end if;
                end loop;
                if newOffer.pairs.Length > 0 then
                    for receiver_index of Nqueue(myindex) loop
                       receivers_tasks(receiver_index).SendOffer(newOffer);
                       msg := To_Unbounded_String(NodesRange'Image(myindex) & " wysyla oferte do " & NodesRange'Image(receiver_index) & " zawierajaca: ");
                        for pair of newOffer.pairs loop
                           msg := msg & " {" & NodesRange'Image(pair.vertex_index) & " " & Integer'Image(pair.total_cost) & " }";
                        end loop;
                        msg := msg & " { do_wierzchołka, koszt}";
                       Put_line(To_string(msg));
                    end loop;
                end if;
            end loop;
        end sender;


        ------- RECEIVER
        task body receiver is
            myindex : NodesRange;
            tmpRow : RoutingRowArray; 
            tmpOffer : Offer;
            tmpNewCost : Integer;
            msg : Unbounded_String;
        begin
            accept Start(idx : NodesRange) do
                myindex := idx;
            end Start;
            loop
                accept SendOffer(newOffer : Offer) do
                    tmpOffer := newOffer;
                end SendOffer;

                msg := To_Unbounded_String(NodesRange'Image(myindex) & " otrzymuje oferte od " & NodesRange'Image(tmpOffer.sender_index) & " zawierajaca: ");
                for pair of tmpOffer.pairs loop
                    msg := msg & " {" & NodesRange'Image(pair.vertex_index) & " " & Integer'Image(pair.total_cost) & " }";
                end loop;
                Put_line(To_string(msg));
                for pair of tmpOffer.pairs loop
                    tmpNewCost := pair.total_cost + 1;
                    tmpRow := R_table(myindex).Get;
                    if tmpNewCost < tmpRow(pair.vertex_index).cost then
                        Put_line(NodesRange'Image(myindex) & " zmienia wpis dla " & NodesRange'Image(pair.vertex_index) 
                            & " na: {" & NodesRange'Image(tmpOffer.sender_index) & " " & Integer'Image(tmpNewCost) & " True }");
                        R_table(myindex).Set(
                            Routing_cell'(next_hop => tmpOffer.sender_index, cost => tmpNewCost, changed => True),
                            pair.vertex_index);
                    end if;
                end loop;
            end loop;
        end receiver;
    begin
        for i in NodesRange loop
           receivers_tasks(i).Start(i);
           senders_tasks(i).Start(i);
        end loop;
        delay 20.0;
        
        declare
            tmp : Routing_cell;
        begin
            --wypisanie routing table
            for i in NodesRange loop
                Put(NodesRange'Image(i));
                Put(": ");
                for ii in NodesRange loop
                    tmp := R_table(i).Get(ii);
                    Put(NodesRange'Image(tmp.next_hop));
                    Put(Integer'Image(tmp.cost));
                    Put(" ");
                    Put(Boolean'Image(tmp.changed));
                    Put(";");
                end loop;
                Put_line("");
            end loop;
        end;
    end;
end zad1;
