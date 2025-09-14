+++
title = "Transportation problem"
description = ""
date = 2019-05-18T18:22:37Z
aliases = []
[extra]
id = 13633
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "1c",
  "d",
  "glagol",
  "go",
  "j",
  "java",
  "julia",
  "kotlin",
  "pascal",
  "perl_6",
  "phix",
  "racket",
  "related_tasks",
  "sas",
]
+++

The '''[[wp:Transportation_theory_%28mathematics%29|transportation problem]]''' in linear programming is to find the optimal transportation plan for certain volumes of resources from suppliers to consumers, taking into account the cost of transportation. The plan is a table (matrix), whose rows and columns correspond to the suppliers and consumers, the cells are placed in cargo volume.

Example of the transportation problem:




{| border="1" style="border-collapse: collapse; border: 1px solid black; text-align: center;"
|-
!style="width:100pt"|
!style="width:100pt"|''Consumer 1'', <br />need 20 kg
!style="width:100pt"|''Consumer 2'', <br />need 30 kg
!style="width:100pt"|''Consumer 3'', <br />need 10 kg
|-
!''Supplier 1'', <br />supply 25 kg
|$3 per kg
|$5 per kg
|$7 per kg
|-
!''Supplier 2'', <br />supply 35 kg
|$3 per kg
|$2 per kg
|$5 per kg
|}




The object is to solve the classical transport problem using the method of potentials (with redistributive cycle) with the preparation of the initial transportation plan by the north-west corner of the features to be implemented in this task. The input is the number of suppliers and customers, inventory levels, needs and cost matrix transport cargo. The output of the program is the optimal plan. If necessary, enter a fictitious vendor or customer.

The solution for the above example would be the plan:




{| border="1" style="border-collapse: collapse; border: 1px solid black; text-align: center;"
|-
!style="width:100pt"|
!style="width:100pt"|''Consumer 1''
!style="width:100pt"|''Consumer 2''
!style="width:100pt"|''Consumer 3''
|-
!''Supplier 1''
| 20 kg
| -
| 5 kg
|-
!''Supplier 2''
| -
| 30 kg
| 5 kg
|}




## See also

* [http://orms.pef.czu.cz/text/transProblem.html The Transportation Problem]
* [https://www.youtube.com/watch?v=BI1SsbDg0vQ&list=PLlCWmLrQuBh1yHhpDfULpRGoTuLgeDB4X| Transportation model - Concepts (youtube)]




## Related tasks

* [[Vogel's approximation method]]





## 1C

<lang>// based on the program of <romix>

перем m,n; // Table size
перем u,v;
перем БазисныеЯчейки;
перем iЦикл, jЦикл;
перем Цены, Спрос, Предложение, Отгрузки; // Arrays of the transportation problem
перем i1, j1;
перем СпросОстаток, ПредложениеОстаток;
перем гл_сч;
перем гсч;

Функция РаспределениеМетодомСевероЗападногоУгла()
    Для j=1 по n Цикл
        СпросОстаток[j]=Спрос[j];
    КонецЦикла;
    Для i=1 по m Цикл
        ПредложениеОстаток[i]=Предложение[i];
    КонецЦикла;
    Для i=1 по m Цикл
        Для j=1 по n Цикл
            БазисныеЯчейки[i][j]=0;
            Отгрузки[i][j]=0;
        КонецЦикла;    
    КонецЦикла;
    Для i=1 по m Цикл
        Для j=1 по n Цикл
            Если ПредложениеОстаток[i]=0 Тогда
                Прервать;
            ИначеЕсли ПредложениеОстаток[i]<0 Тогда
                ВызватьИсключение("Error: balance of the offer less than 0");
            КонецЕсли;
            чОбъем=СпросОстаток[j];
            Если чОбъем=0 Тогда
                Продолжить;
            ИначеЕсли чОбъем<0 Тогда    
                ВызватьИсключение("Error: balance of the demand less than 0");
            КонецЕсли;
            Если ПредложениеОстаток[i]<чОбъем Тогда
                чОбъем=ПредложениеОстаток[i];
            КонецЕсли; 
            СпросОстаток[j]=СпросОстаток[j]-чОбъем;
            ПредложениеОстаток[i]=ПредложениеОстаток[i]-чОбъем;
            БазисныеЯчейки[i][j]=1;
            Отгрузки[i][j]=чОбъем;
        КонецЦикла;    
    КонецЦикла;    
КонецФункции

Функция ПроверкаПравильностиОтгрузок()
    Для i=1 по m Цикл
        стр="Отгрузки: ";
        Для j=1 по n Цикл
            стр=стр+Отгрузки[i][j]+" ";
        КонецЦикла;    
        Сообщить(стр);
    КонецЦикла;        
    Для i=1 по m Цикл
        чОбъем=0;
        Для j=1 по n Цикл
            чОбъем=чОбъем+Отгрузки[i][j];
        КонецЦикла;    
        Если чОбъем<>Предложение[i] Тогда
            ВызватьИсключение("Error: shipment on the line does not equal the proposal in the row "+i);
        КонецЕсли;
    КонецЦикла;    
    Для j=1 по n Цикл
        чОбъем=0;
        Для i=1 по m Цикл
            чОбъем=чОбъем+Отгрузки[i][j];
        КонецЦикла;    
        Если чОбъем<>Спрос[j] Тогда
            ВызватьИсключение("Error: shipment by the column does not equal to the demand in the column "+j);
        КонецЕсли;
    КонецЦикла;    
    Возврат Истина;
КонецФункции    

Функция ВычислениеПотенциалов()
    перем i, j;
    Для i=1 по m Цикл
        u[i]=НеОпределено;
    КонецЦикла;
    Для j=1 по n Цикл
        v[j]=НеОпределено;
    КонецЦикла;
    u[1]=0;
    гл_сч=m*n;
    ВычислениеПотенциаловПоГоризонтали(1);
    Для i=1 по m Цикл
        Если u[i]=НеОпределено Тогда
            Сообщить("Failed to evaluate the potential u["+i+"]");
            Возврат Ложь;
        КонецЕсли;    
    КонецЦикла;
    Для j=1 по n Цикл
        Если v[j]=НеОпределено Тогда
            Сообщить("Failed to evaluate the potential v["+j+"]");
            Возврат Ложь;
        КонецЕсли;    
    КонецЦикла;
    Возврат Истина;
КонецФункции

Функция ВычислениеПотенциаловПоВертикали(j)
    Если v[j]=НеОпределено Тогда 
        ВызватьИсключение("Failed to get the potential v["+j+"]");
    КонецЕсли;    
    Для i=1 по m Цикл
        Если БазисныеЯчейки[i][j]=0 Тогда
            Продолжить;
        КонецЕсли;
        Если u[i]<>НеОпределено Тогда
            Продолжить;
        Иначе
            u[i]=Цены[i][j]-v[j];
            ВычислениеПотенциаловПоГоризонтали(i);
        КонецЕсли;    
    КонецЦикла;    
КонецФункции

Функция ВычислениеПотенциаловПоГоризонтали(i)
    гл_сч=гл_сч-1;
    Если гл_сч=0 Тогда
        ВызватьИсключение("Looping in the calculation of potential");
    КонецЕсли;    
    Если u[i]=НеОпределено Тогда 
        ВызватьИсключение("Failed to get potential u["+i+"]");
    КонецЕсли;    
    Для j=1 по n Цикл
        Если БазисныеЯчейки[i][j]=0 Тогда
            Продолжить;
        КонецЕсли;
        Если v[j]<>НеОпределено Тогда
            Продолжить;
        Иначе
            v[j]=Цены[i][j]-u[i];
            ВычислениеПотенциаловПоВертикали(j);
        КонецЕсли;    
    КонецЦикла;    
КонецФункции    

Функция ПроверкаОптимальности()
    перем чРешениеОптимально, чМинимальнаяДельта, i, j, Дельта;
    чРешениеОптимально=Истина;
    чМинимальнаяДельта=НеОпределено;
    Для i=1 по m Цикл
        стр="Дельта=";
        Для j=1 по n Цикл
            Если БазисныеЯчейки[i][j]=1 Тогда
                Дельта=0;
            Иначе
                Дельта = Цены[i][j]-u[i]-v[j];
            КонецЕсли;
            стр=стр+Дельта+" ";
            Если Дельта<0 Тогда
                чРешениеОптимально=Ложь;
            КонецЕсли;
            Если чМинимальнаяДельта=НеОпределено Тогда
                чМинимальнаяДельта=Дельта;
                i1=i;
                j1=j;
            Иначе
                Если Дельта<чМинимальнаяДельта Тогда
                    чМинимальнаяДельта=Дельта;
                    i1=i;
                    j1=j;
                КонецЕсли;        
            КонецЕсли;    
        КонецЦикла;    
    КонецЦикла;
    Возврат чРешениеОптимально;
КонецФункции

Функция СтоимостьПеревозки()
    чСумма=0;
    Для i=1 по m Цикл
        Для j=1 по n Цикл
            чСумма=чСумма+(Отгрузки[i][j]*Цены[i][j]);
        КонецЦикла;    
    КонецЦикла;
    Возврат чСумма;
КонецФункции    

Функция ПоискНулевойЯчейкиДляВводаВБазис()
    ок=0;
    Для i=1 по m Цикл
        Для j=1 по n Цикл
            Если БазисныеЯчейки[i][j]=0 Тогда
                ок=1;
                Прервать;
            КонецЕсли;
        КонецЦикла;
        Если ок=1 Тогда
            Прервать;
        КонецЕсли;
    КонецЦикла;    
    Если ок=0 Тогда
        ВызватьИсключение("There is no nonbasic (zero) cell entry into the basis");
    КонецЕсли;
    Пока 1=1 Цикл
        i=ГСЧ.СлучайноеЧисло(1, m);
        j=ГСЧ.СлучайноеЧисло(1, n);
        Если БазисныеЯчейки[i][j]=1 Тогда
            Продолжить;
        КонецЕсли;
        Если Отгрузки[i][j]<>0 Тогда
            ВызватьИсключение("Nonzero shipment for nonbasic cell");
        КонецЕсли;
        БазисныеЯчейки[i][j]=1;
        Сообщить("В базис введена ячейка "+i+" "+j);
        Возврат Истина;
    КонецЦикла;
КонецФункции

Функция НайтиЦикл(i0, j0)
    гл_сч = m*n;
    iЦикл.Очистить();
    jЦикл.Очистить();
    Если НайтиЦикл_ПоГоризонтали(i0, j0) Тогда
        Возврат Истина;
    КонецЕсли;
    Возврат Ложь;
КонецФункции

Функция НайтиЦикл_ПоГоризонтали(i0, j0)
    гл_сч=гл_сч-1;
    Если гл_сч=0 Тогда
        ВызватьИсключение("Too many iterations in the cycle search");
    КонецЕсли;    
    Для j=1 по n Цикл
        Если j=j0 Тогда
            Продолжить;
        КонецЕсли;
        Если БазисныеЯчейки[i0][j]=0 Тогда
            Продолжить;
        КонецЕсли;
        Если НайтиЦикл_ПоВертикали(i0, j) Тогда
            iЦикл.Добавить(i0);
            jЦикл.Добавить(j);
            Возврат Истина;
        КонецЕсли;    
    КонецЦикла;
    Возврат Ложь;
КонецФункции    

Функция НайтиЦикл_ПоВертикали(i0, j0)
    Для i=1 по m Цикл
        Если (j0=j1) и (i=i1) Тогда
                iЦикл.Добавить(i);
                jЦикл.Добавить(j0);
                Возврат Истина;
        КонецЕсли;    
        Если i=i0 Тогда
            Продолжить;
        КонецЕсли;
        Если БазисныеЯчейки[i][j0]=0 Тогда
            Продолжить;
        КонецЕсли;
        Если НайтиЦикл_ПоГоризонтали(i, j0) Тогда
            iЦикл.Добавить(i);
            jЦикл.Добавить(j0);
            Возврат Истина;
        КонецЕсли;    
    КонецЦикла;    
    Возврат Ложь;
КонецФункции

Функция ПерераспределениеПоЦиклу()
    Сообщить("Redistribution by the cycle "+iЦикл.Количество());
    Если jЦикл.Количество()<>iЦикл.Количество() Тогда
        ВызватьИсключение("Unequal dimension for the cycle coordinates");
    КонецЕсли;
    Если iЦикл.Количество()<4 Тогда
        ВызватьИсключение("Cycle is less than 4 items");
    КонецЕсли;    
    Тета=НеОпределено;
    Знак="+";
    Для й=0 по iЦикл.ВГраница() Цикл
        i=iЦикл[й];
        j=jЦикл[й];
        Если Знак="-" Тогда
            Объем=Отгрузки[i][j];
            Если Тета=НеОпределено Тогда
                Тета=Объем;
            Иначе
                Если Объем<Тета Тогда
                    Тета=Объем;
                КонецЕсли;    
            КонецЕсли;    
            Знак="+";
        Иначе
            Знак="-";
        КонецЕсли;    
    КонецЦикла;    
    Если Тета=НеОпределено Тогда
        ВызватьИсключение("Failed to evaluate variable theta.");
    КонецЕсли;
    Сообщить("Тета="+Тета);
    Если Тета=0 Тогда
        Возврат Ложь;
    КонецЕсли;
    Знак="+";
    Для й=0 по iЦикл.ВГраница() Цикл
        i=iЦикл[й];
        j=jЦикл[й];
        Если Знак="-" Тогда
            Отгрузки[i][j]=Отгрузки[i][j]-Тета;
            Знак="+";
        Иначе
            Отгрузки[i][j]=Отгрузки[i][j]+Тета;
            Знак="-";
        КонецЕсли;    
    КонецЦикла;
    Возврат Истина;
КонецФункции

Функция РешениеТранспортнойЗадачи()
    ГСЧ = Новый ГенераторСлучайныхЧисел();
    БазисныеЯчейки = Новый Массив(m+1,n+1);
    Отгрузки = Новый Массив(m+1,n+1);
    СпросОстаток=Новый Массив(n+1);
    ПредложениеОстаток=Новый Массив(m+1);
    u=Новый Массив(m+1);
    v=Новый Массив(n+1);
    iЦикл = Новый Массив;
    jЦикл = Новый Массив;
    чСпрос=0;
    Для j=1 по n Цикл
        чСпрос=чСпрос+Спрос[j];
    КонецЦикла;    
    чПредложение=0;
    Для i=1 по m Цикл
        чПредложение=чПредложение+Предложение[i];
    КонецЦикла;
    Если чПредложение>чСпрос Тогда
        Сообщить("Offering more than the demand for "+(чПредложение-чСпрос)+" units of cargo. Create a fictitious user.");
        Возврат Ложь;
    ИначеЕсли чПредложение<чСпрос Тогда
        Сообщить("Offering less than the demand for "+(чСпрос-чПредложение)+" units of cargo. Create a fictitious vendor.");
        Возврат Ложь;
    КонецЕсли;        
    РаспределениеМетодомСевероЗападногоУгла();
    чСумма=СтоимостьПеревозки();
    Сообщить("The cost of transportation by the north-west corner: "+чСумма);
    Пока 1=1 Цикл
        ПроверкаПравильностиОтгрузок();
        счБазисных=0;
        Для i=1 по m Цикл
            Для j=1 по n Цикл
                Если Отгрузки[i][j]>0 Тогда
                    БазисныеЯчейки[i][j]=1;
                    счБазисных=счБазисных+1;
                ИначеЕсли Отгрузки[i][j]<0 Тогда    
                    ВызватьИсключение("Shipments should not be negative");
                Иначе
                    БазисныеЯчейки[i][j]=0;
                КонецЕсли;    
            КонецЦикла;    
        КонецЦикла;
        Пока счБазисных<(m+n-1) Цикл
            Сообщить("Решение вырождено");
            ПоискНулевойЯчейкиДляВводаВБазис();
            счБазисных=счБазисных+1;
        КонецЦикла;
        Если ВычислениеПотенциалов()=Ложь Тогда
            Продолжить;
        КонецЕсли;    
        Если ПроверкаОптимальности()=Истина Тогда
            Сообщить("Solution is optimal.");
            Прервать;
        КонецЕсли;
        Сообщить("Solution is not optimal.");
        Если НайтиЦикл(i1, j1)= Ложь Тогда
            ВызватьИсключение("Unable to find a cycle");
        КонецЕсли;
        ПерераспределениеПоЦиклу();
        чСумма=СтоимостьПеревозки();
        Сообщить("***");
        Сообщить("The cost of transport: "+чСумма);
    КонецЦикла;    
    Возврат Истина;
КонецФункции

&НаКлиенте
Процедура КомандаРассчитать(Команда)
    РешениеТранспортнойЗадачи();
КонецПроцедуры
```



## D

```d
import std.stdio, std.range, std.algorithm, std.conv, std.math, std.traits;

final class Shipment {
    double quantity;
    immutable double costPerUnit;
    immutable size_t r, c;

    this(in double q, in double cpu, in size_t r_, in size_t c_)
    pure nothrow @safe @nogc {
        quantity = q;
        costPerUnit = cpu;
        this.r = r_;
        this.c = c_;
    }
}

alias ShipmentMat = Shipment[][];
alias CostsMat = double[][];

void init(in string fileName, out uint[] demand, out uint[] supply,
          out CostsMat costs, out ShipmentMat matrix) {
    auto inParts = fileName.File.byLine.map!splitter.joiner;

    immutable numSources = inParts.front.to!uint;
    inParts.popFront;
    immutable numDestinations = inParts.front.to!uint;
    inParts.popFront;

    foreach (immutable i; 0 .. numSources) {
        supply ~= inParts.front.to!uint;
        inParts.popFront;
    }

    foreach (immutable i; 0 .. numDestinations) {
        demand ~= inParts.front.to!uint;
        inParts.popFront;
    }

    // Fix imbalance.
    immutable totalSrc = supply.sum;
    immutable totalDst = demand.sum;

    if (totalSrc > totalDst)
        demand ~= totalSrc - totalDst;
    else if (totalDst > totalSrc)
        supply ~= totalDst - totalSrc;

    costs = new CostsMat(supply.length, demand.length);
    foreach (row; costs)
        row[] = 0.0;
    matrix = new ShipmentMat(supply.length, demand.length);

    foreach (immutable i; 0 .. numSources)
        foreach (immutable j; 0 .. numDestinations) {
            costs[i][j] = inParts.front.to!double;
            inParts.popFront;
        }
}

void northWestCornerRule(uint[] demand, uint[] supply, in CostsMat costs,
                         ShipmentMat matrix) pure nothrow @safe {
    size_t northwest = 0;
    foreach (immutable r; 0 .. supply.length) {
        foreach (immutable c; northwest .. demand.length) {
            immutable quantity = min(supply[r], demand[c]);
            if (quantity > 0) {
                matrix[r][c] = new Shipment(quantity, costs[r][c], r, c);

                supply[r] -= quantity;
                demand[c] -= quantity;

                if (supply[r] == 0) {
                    northwest = c;
                    break;
                }
            }
        }
    }
}

void steppingStone(in uint[] demand, in uint[] supply,
                   in CostsMat costs, ShipmentMat matrix) pure @safe {
    double maxReduction = 0;
    Shipment[] move;
    Shipment leaving = null;

    fixDegenerateCase(demand, supply, costs, matrix);

    foreach (immutable r; 0 .. supply.length) {
        foreach (immutable c; 0 .. demand.length) {
            if (matrix[r][c] !is null)
                continue;

            auto trial = new Shipment(0, costs[r][c], r, c);
            auto path = getClosedPath(trial, matrix);

            double reduction = 0;
            double lowestQuantity = uint.max;
            Shipment leavingCandidate = null;

            bool plus = true;
            foreach (s; path) {
                if (plus) {
                    reduction += s.costPerUnit;
                } else {
                    reduction -= s.costPerUnit;
                    if (s.quantity < lowestQuantity) {
                        leavingCandidate = s;
                        lowestQuantity = s.quantity;
                    }
                }
                plus = !plus;
            }
            if (reduction < maxReduction) {
                move = path;
                leaving = leavingCandidate;
                maxReduction = reduction;
            }
        }
    }

    if (move !is null) {
        auto q = leaving.quantity;
        auto plus = true;
        foreach (s; move) {
            s.quantity += plus ? q : -q;
            matrix[s.r][s.c] = (s.quantity == 0) ? null : s;
            plus = !plus;
        }
        steppingStone(demand, supply, costs, matrix);
    }
}

auto matrixToSeq(ShipmentMat matrix) pure nothrow @nogc @safe {
    return matrix.joiner.filter!(s => s !is null);
}

Shipment[] getClosedPath(Shipment s, ShipmentMat matrix) pure @safe
in {
    assert(s !is null);
} out(result) {
    assert(result.all!(sh => sh !is null));
} body {
    Shipment[] stones = chain([s], matrixToSeq(matrix)).array;

    // Remove (and keep removing) elements that do not have
    // a vertical AND horizontal neighbor.
    while (true) {
        auto stones2 = stones.remove!((in e) {
            const nbrs = getNeighbors(e, stones);
            return nbrs[0] is null || nbrs[1] is null;
        });

        if (stones2.length == stones.length)
            break;
        stones = stones2;
    }

    // Place the remaining elements in the correct plus-minus order.
    auto stones3 = stones.dup;
    Shipment prev = s;
    foreach (immutable i, ref si; stones3) {
        si = prev;
        prev = getNeighbors(prev, stones)[i % 2];
    }
    return stones3;
}

Shipment[2] getNeighbors(ShipmentsRange)(in Shipment s, ShipmentsRange seq)
pure nothrow @safe @nogc
if (isForwardRange!ShipmentsRange && is(ForeachType!ShipmentsRange == Shipment))
in {
    assert(s !is null);
    assert(seq.all!(sh => sh !is null));
} body {
    Shipment[2] nbrs;

    foreach (o; seq) {
        if (o !is s) {
            if (o.r == s.r && nbrs[0] is null)
                nbrs[0] = o;
            else if (o.c == s.c && nbrs[1] is null)
                nbrs[1] = o;
            if (nbrs[0] !is null && nbrs[1] !is null)
                break;
        }
    }

    return nbrs;
}

void fixDegenerateCase(in uint[] demand, in uint[] supply,
                       in CostsMat costs, ShipmentMat matrix) pure @safe {
    immutable eps = double.min_normal;

    if (supply.length.signed + demand.length.signed - 1 != matrixToSeq(matrix).walkLength) {
        foreach (immutable r; 0 .. supply.length) {
            foreach (immutable c; 0 .. demand.length) {
                if (matrix[r][c] is null) {
                    auto dummy = new Shipment(eps, costs[r][c], r, c);
                    if (getClosedPath(dummy, matrix).length == 0) {
                        matrix[r][c] = dummy;
                        return;
                    }
                }
            }
        }
    }
}

void printResult(in string fileName, in uint[] demand, in uint[] supply,
                 in CostsMat costs, in ShipmentMat matrix) @safe /*@nogc*/ {
    writefln("Optimal solution %s", fileName);
    double totalCosts = 0;

    foreach (immutable r; 0 .. supply.length) {
        foreach (immutable c; 0 .. demand.length) {
            const s = matrix[r][c];
            if (s !is null && s.r == r && s.c == c) {
                writef(" %3d ", cast(uint)s.quantity);
                totalCosts += s.quantity * s.costPerUnit;
            } else
                write("  -  ");
        }
        //writeln; // Not @safe?
        write('\n');
    }
    writefln("\nTotal costs: %s\n", totalCosts);
}

void main() {
    foreach (fileName; ["transportation_problem1.txt",
                        "transportation_problem2.txt",
                        "transportation_problem3.txt"]) {
        uint[] demand, supply;
        CostsMat costs;
        ShipmentMat matrix;
        init(fileName, demand, supply, costs, matrix);
        northWestCornerRule(demand, supply, costs, matrix);
        steppingStone(demand, supply, costs, matrix);
        printResult(fileName, demand, supply, costs, matrix);
    }
}
```

```txt
Optimal solution transportation_problem1.txt
  20   -     5 
  -    30    5 

Total costs: 180

Optimal solution transportation_problem2.txt
  -    -    -    12 
  20   -    10   10 
  -    30   -     3 

Total costs: 130

Optimal solution transportation_problem3.txt
  -    -    -    14 
  -     9   -     1 
  10   -     5   -  
  -     5    7   -  
  -     1   -    -  

Total costs: 1000
```



## Glagol

<lang>ОТДЕЛ Транспорт+;
ИСПОЛЬЗУЕТ
  Вывод ИЗ "...\Отделы\Обмен\",
  Приём;

ПЕР
  Поставщиков, Потребителей: ЦЕЛ;
  Запасы, Потребности: ДОСТУП К РЯД ИЗ ЦЕЛ;
  Расходы, План: ДОСТУП К РЯД ИЗ РЯД ИЗ ЦЕЛ;
  U, V: ДОСТУП К РЯД ИЗ ЦЕЛ;
  оцСв: ДОСТУП К РЯД ИЗ НАБОР значение: ЦЕЛ; поставщик, потребитель: ЦЕЛ КОН;
  начQ_поставщик, начQ_потребитель: ЦЕЛ;
  Q: ДОСТУП К РЯД ИЗ РЯД ИЗ УЗКЦЕЛ;
  Поправка, Разница: ЦЕЛ;

ЗАДАЧА ПринятьДанные;
ПЕР
  сч1, сч2: ЦЕЛ;
  сумма1, сумма2, разница: ЦЕЛ;
  памЗап, памПотр: ДОСТУП К РЯД ИЗ ЦЕЛ;
УКАЗ
  Вывод.Цепь("Number of suppliers: ");
  Поставщиков := Приём.Число();
  Вывод.Цепь(".^Number of consumers: ");
  Потребителей := Приём.Число();
  СОЗДАТЬ(памЗап, Поставщиков);
  СОЗДАТЬ(памПотр, Потребителей);
  Вывод.Цепь(".^Inventories^^");
  ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
    памЗап[сч1] := Приём.Число();
    Вывод.Цепь(" ")
  КОН;
  Вывод.Цепь("^Requirements:^");
  ОТ сч1 := 0 ДО Потребителей-1 ВЫП
    памПотр[сч1] := Приём.Число();
    Вывод.Цепь(" ")
  КОН;
  сумма1 := 0;
  ОТ сч1 := 0 ДО Поставщиков-1 ВЫП УВЕЛИЧИТЬ(сумма1, памЗап[сч1]) КОН;
  сумма2 := 0;
  ОТ сч1 := 0 ДО Потребителей-1 ВЫП УВЕЛИЧИТЬ(сумма2, памПотр[сч1]) КОН;
  ЕСЛИ сумма1 > сумма2 ТО
    разница := сумма1 - сумма2;
    Вывод.ЧЦел("^Introduced a fictitious consumer.", сумма1, сумма2, разница, 0);
    УВЕЛИЧИТЬ(Потребителей);
    СОЗДАТЬ(Потребности, Потребителей);
    ОТ сч1 := 0 ДО Потребителей-2 ВЫП Потребности[сч1] := памПотр[сч1] КОН;
    Потребности[Потребителей-1] := разница;
    СОЗДАТЬ(Запасы, Поставщиков);
    ОТ сч1 := 0 ДО Поставщиков-1 ВЫП Запасы[сч1] := памЗап[сч1] КОН
  АЕСЛИ сумма2 > сумма1 ТО
    разница := сумма2 - сумма1;
    Вывод.ЧЦел("^Introduced a fictitious supplier.", сумма2, сумма1, разница, 0);
    УВЕЛИЧИТЬ(Поставщиков);
    СОЗДАТЬ(Запасы, Поставщиков);
    ОТ сч1 := 0 ДО Поставщиков-2 ВЫП Запасы[сч1] := памЗап[сч1] КОН;
    Запасы[Поставщиков-1] := разница;
    СОЗДАТЬ(Потребности, Потребителей);
    ОТ сч1 := 0 ДО Потребителей-1 ВЫП Потребности[сч1] := памПотр[сч1] КОН
  ИНАЧЕ
    СОЗДАТЬ(Запасы, Поставщиков);
    ОТ сч1 := 0 ДО Поставщиков-1 ВЫП Запасы[сч1] := памЗап[сч1] КОН;
    СОЗДАТЬ(Потребности, Потребителей);
    ОТ сч1 := 0 ДО Потребителей-1 ВЫП Потребности[сч1] := памПотр[сч1] КОН
  КОН;
  СОЗДАТЬ(Расходы, Поставщиков, Потребителей);
  Вывод.Цепь("^The matrix of costs:^");
  ЕСЛИ сумма1 > сумма2 ТО
    ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
      ОТ сч2 := 0 ДО Потребителей-2 ВЫП
        Расходы[сч1, сч2] := Приём.Число();
        Вывод.Цепь(" ")
      КОН;
      Расходы[сч1, Потребителей-1] := 0;
      Вывод.Цепь("^")
    КОН
  АЕСЛИ сумма2 > сумма1 ТО
    ОТ сч1 := 0 ДО Поставщиков-2 ВЫП
      ОТ сч2 := 0 ДО Потребителей-1 ВЫП
        Расходы[сч1, сч2] := Приём.Число();
        Вывод.Цепь(" ")
      КОН;
      Вывод.Цепь("^")
    КОН;
    ОТ сч1 := 0 ДО Потребителей-1 ВЫП Расходы[Поставщиков-1, сч1] := 0 КОН
  ИНАЧЕ
    ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
      ОТ сч2 := 0 ДО Потребителей-1 ВЫП
        Расходы[сч1, сч2] := Приём.Число();
        Вывод.Цепь(" ")
      КОН;
      Вывод.Цепь("^")
    КОН
  КОН;
  СОЗДАТЬ(План, Поставщиков, Потребителей);
  СОЗДАТЬ(U, Поставщиков);
  СОЗДАТЬ(V, Потребителей);
  СОЗДАТЬ(оцСв, Потребителей*Поставщиков-(Потребителей+Поставщиков-1));
  СОЗДАТЬ(Q, Поставщиков, Потребителей)
КОН ПринятьДанные;

ЗАДАЧА ВывестиПлан;
ПЕР
  сч1, сч2: ЦЕЛ;
УКАЗ
  ОТ сч1 := 1 ДО Потребителей ВЫП Вывод.Цепь("-----") КОН;
  Вывод.Цепь("^");
  ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
    ОТ сч2 := 0 ДО Потребителей-1 ВЫП
      ЕСЛИ План[сч1, сч2] = -1 ТО Вывод.Цепь("  -  ") ИНАЧЕ
        Вывод.ЧЦел("%4d ", План[сч1, сч2], 0, 0, 0);
      КОН
    КОН;
    Вывод.Цепь("^")
  КОН;
  ОТ сч1 := 1 ДО Потребителей ВЫП Вывод.Цепь("-----") КОН
КОН ВывестиПлан;

ЗАДАЧА ПосчитатьПоправку;
ПЕР
  сч1, сч2: ЦЕЛ;
УКАЗ
  Поправка := -1;
  ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
    ОТ сч2 := 0 ДО Потребителей-1 ВЫП
      ЕСЛИ План[сч1, сч2] # -1 ТО
        ЕСЛИ Q[сч1, сч2] = -1 ТО
          ЕСЛИ Поправка = -1 ТО Поправка := План[сч1, сч2]
          АЕСЛИ Поправка > План[сч1, сч2] ТО Поправка := План[сч1, сч2] КОН
        КОН
      КОН
    КОН
  КОН;
  Разница := Разница * Поправка
КОН ПосчитатьПоправку;

ЗАДАЧА РасставитьНули(недостаток: ЦЕЛ);
ПЕР
  Связь: ДОСТУП К РЯД ИЗ РЯД ИЗ УЗКЦЕЛ;
  сч1, сч2: ЦЕЛ;
  естьБезСвязи: КЛЮЧ;

  ЗАДАЧА ЕстьНапротив(строка, столбец: ЦЕЛ): КЛЮЧ;
  ПЕР сч: ЦЕЛ;
  УКАЗ
    ОТ сч := 0 ДО Поставщиков-1 ВЫП
      ЕСЛИ (сч # строка) И (Связь[сч, столбец] = 1) ТО ВОЗВРАТ ВКЛ КОН
    КОН;
    ОТ сч := 0 ДО Потребителей-1 ВЫП
      ЕСЛИ (сч # столбец) И (Связь[строка, сч] = 1) ТО ВОЗВРАТ ВКЛ КОН
    КОН;
    ВОЗВРАТ ОТКЛ
  КОН ЕстьНапротив;

  ЗАДАЧА СтолбецБезСвязи(номер: ЦЕЛ): КЛЮЧ;
  ПЕР сч: ЦЕЛ;
  УКАЗ
    ОТ сч := 0 ДО Поставщиков-1 ВЫП
      ЕСЛИ Связь[сч, номер] = 1 ТО ВОЗВРАТ ОТКЛ КОН
    КОН;
    ВОЗВРАТ ВКЛ
  КОН СтолбецБезСвязи;

УКАЗ
  СОЗДАТЬ(Связь, Поставщиков, Потребителей);
  естьБезСвязи := ОТКЛ;
  ОТ сч1 := 0 ДО Потребителей-1 ВЫП
    ЕСЛИ План[0, сч1] = -1 ТО Связь[0, сч1] := -1 ИНАЧЕ Связь[0, сч1] := 1 КОН
  КОН;
  ОТ сч1 := 1 ДО Поставщиков-1 ВЫП
    ОТ сч2 := 0 ДО Потребителей-1 ВЫП
      ЕСЛИ План[сч1, сч2] = -1 ТО Связь[сч1, сч2] := -1 ИНАЧЕ Связь[сч1, сч2] := 0 КОН
    КОН
  КОН;
  ОТ сч1 := 1 ДО Поставщиков-1 ВЫП
    ОТ сч2 := 0 ДО Потребителей-1 ВЫП
      ЕСЛИ Связь[сч1, сч2] = 0 ТО
        ЕСЛИ ЕстьНапротив(сч1, сч2) ТО Связь[сч1, сч2] := 1
        АЕСЛИ НЕ естьБезСвязи ТО естьБезСвязи := ВКЛ КОН
      КОН
    КОН
  КОН;
  ЕСЛИ естьБезСвязи ТО
    ОТ сч1 := Поставщиков-1 ДО 1 ПО -1 ВЫП
      ОТ сч2 := Потребителей-1 ДО 0 ПО -1 ВЫП
        ЕСЛИ Связь[сч1, сч2] = 0 ТО
          ЕСЛИ ЕстьНапротив(сч1, сч2) ТО Связь[сч1, сч2] := 1
          АЕСЛИ НЕ естьБезСвязи ТО естьБезСвязи := ВКЛ КОН
        КОН
      КОН
    КОН
  КОН;
  ЕСЛИ естьБезСвязи ТО
    ОТ сч1 := 0 ДО Потребителей-1 ВЫП
      ЕСЛИ СтолбецБезСвязи(сч1) ТО План[0, сч1] := 0; УМЕНЬШИТЬ(недостаток) КОН;
      ЕСЛИ недостаток = 0 ТО сч1 := Потребителей КОН
    КОН
  КОН;
  КОЛЬЦО
    ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
      ОТ сч2 := 0 ДО Потребителей-1 ВЫП
        ЕСЛИ недостаток = 0 ТО ВЫХОД ИНАЧЕ
          ЕСЛИ План[сч1, сч2] = -1 ТО
            План[сч1, сч2] := 0; УМЕНЬШИТЬ(недостаток);
          КОН
        КОН
      КОН
    КОН;
    ВЫХОД
  КОН
КОН РасставитьНули;

ЗАДАЧА ЗаполнитьОтУгла;
ПЕР
  ОсталосьВНаличии, ОсталосьПотребным: ДОСТУП К РЯД ИЗ ЦЕЛ;
  занято, недостаток: ЦЕЛ;
  сч1, сч2: ЦЕЛ;
УКАЗ
  СОЗДАТЬ(ОсталосьВНаличии, Поставщиков);
  СОЗДАТЬ(ОсталосьПотребным, Потребителей);
  ОТ сч1 := 0 ДО Поставщиков-1 ВЫП ОсталосьВНаличии[сч1] := Запасы[сч1] КОН;
  ОТ сч1 := 0 ДО Потребителей-1 ВЫП ОсталосьПотребным[сч1] := Потребности[сч1] КОН;
  занято := 0;
  ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
    ОТ сч2 := 0 ДО Потребителей-1 ВЫП
      ЕСЛИ ОсталосьВНаличии[сч1] = 0 ТО План[сч1, сч2] := -1 ИНАЧЕ
        ЕСЛИ ОсталосьВНаличии[сч1] > ОсталосьПотребным[сч2] ТО
          ЕСЛИ ОсталосьПотребным[сч2] # 0 ТО План[сч1, сч2] := ОсталосьПотребным[сч2]; УВЕЛИЧИТЬ(занято)
          ИНАЧЕ План[сч1, сч2] := -1 КОН;
          УМЕНЬШИТЬ(ОсталосьВНаличии[сч1], ОсталосьПотребным[сч2]);
          ОсталосьПотребным[сч2] := 0
        ИНАЧЕ
          ЕСЛИ ОсталосьВНаличии[сч1] # 0 ТО План[сч1, сч2] := ОсталосьВНаличии[сч1]; УВЕЛИЧИТЬ(занято)
          ИНАЧЕ План[сч1, сч2] := -1 КОН;
          УМЕНЬШИТЬ(ОсталосьПотребным[сч2], ОсталосьВНаличии[сч1]);
          ОсталосьВНаличии[сч1] := 0
        КОН
      КОН
    КОН
  КОН;
  недостаток := (Поставщиков+Потребителей-1) - занято;
  ЕСЛИ недостаток > 0 ТО РасставитьНули(недостаток) КОН
КОН ЗаполнитьОтУгла;

ЗАДАЧА ОценитьБазисныеКлетки;
ПЕР
  сч1, сч2, сч3: ЦЕЛ;
  суммы: ДОСТУП К РЯД ИЗ РЯД 3 ИЗ ЦЕЛ;
  известно: ДОСТУП К РЯД ИЗ РЯД 2 ИЗ КЛЮЧ;
УКАЗ
  СОЗДАТЬ(суммы, Поставщиков+Потребителей-1);
  СОЗДАТЬ(известно, Поставщиков+Потребителей-1);
  известно[0][0] := ВКЛ; известно[0][1] := ОТКЛ;
  ОТ сч1 := 1 ДО (Поставщиков+Потребителей-1)-1 ВЫП известно[сч1][0] := ОТКЛ; известно[сч1][1] := ОТКЛ КОН;
  сч3 := 0;
  ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
    ОТ сч2 := 0 ДО Потребителей-1 ВЫП
      ЕСЛИ План[сч1, сч2] # -1 ТО
        суммы[сч3][0] := сч1; суммы[сч3][1] := сч2; суммы[сч3][2] := Расходы[сч1, сч2];
        УВЕЛИЧИТЬ(сч3)
      КОН
    КОН
  КОН;
  U[0] := 0;
  ОТ сч1 := 1 ДО (Поставщиков+Потребителей-1)-1 ВЫП
    ЕСЛИ суммы[сч1][0] = 0 ТО известно[сч1][0] := ВКЛ КОН
  КОН;
  сч3 := 0;
  ПОВТОРЯТЬ
    сч1 := 0;
    ПОКА НЕ (известно[сч1][0] # известно[сч1][1]) ВЫП
      УВЕЛИЧИТЬ(сч1)
    КОН;
    ЕСЛИ известно[сч1][0] ТО
      V[суммы[сч1][1]] := суммы[сч1][2] - U[суммы[сч1][0]];
      известно[сч1][1] := ВКЛ;
      ОТ сч2 := 0 ДО (Поставщиков+Потребителей-1)-1 ВЫП
        ЕСЛИ (суммы[сч2][1] = суммы[сч1][1]) И (НЕ известно[сч2][1]) ТО известно[сч2][1] := ВКЛ КОН
      КОН
    ИНАЧЕ
      U[суммы[сч1][0]] := суммы[сч1][2] - V[суммы[сч1][1]];
      известно[сч1][0] := ВКЛ;
      ОТ сч2 := 0 ДО (Поставщиков+Потребителей-1)-1 ВЫП
        ЕСЛИ (суммы[сч2][0] = суммы[сч1][0]) И (НЕ известно[сч2][0]) ТО известно[сч2][0] := ВКЛ КОН
      КОН
    КОН;
    УВЕЛИЧИТЬ(сч3)
  ДО сч3 = Поставщиков+Потребителей-1
КОН ОценитьБазисныеКлетки;

ЗАДАЧА ОценитьСвободныеКлетки(): КЛЮЧ;
ПЕР
  сч1, сч2, сч3: ЦЕЛ;
  естьПолож: КЛЮЧ;
УКАЗ
  естьПолож := ОТКЛ;
  сч3 := 0;
  ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
    ОТ сч2 := 0 ДО Потребителей-1 ВЫП
      ЕСЛИ План[сч1, сч2] = -1 ТО
        оцСв[сч3].значение := U[сч1]+V[сч2]-Расходы[сч1,сч2];
        оцСв[сч3].поставщик := сч1; оцСв[сч3].потребитель := сч2;
        ЕСЛИ оцСв[сч3].значение > 0 ТО естьПолож := ВКЛ КОН;
        УВЕЛИЧИТЬ(сч3)
      КОН
    КОН
  КОН;
  ЕСЛИ естьПолож ТО ВОЗВРАТ ОТКЛ ИНАЧЕ ВОЗВРАТ ВКЛ КОН
КОН ОценитьСвободныеКлетки;

ЗАДАЧА Цикл;
ПЕР
  сч1, сч2, сч3: ЦЕЛ;
  максЗн: ЦЕЛ;
  начало, циклНайден: КЛЮЧ;

  ЗАДАЧА НаЛинии(наКакой: ЦЕЛ; столб: КЛЮЧ): ЦЕЛ;
  ПЕР сч, сколько: ЦЕЛ;
  УКАЗ
    сколько := 0;
    ЕСЛИ столб ТО
      ОТ сч := 0 ДО Поставщиков-1 ВЫП
        ЕСЛИ (План[сч, наКакой] # -1) ИЛИ ((сч = начQ_поставщик) И (наКакой = начQ_потребитель)) ТО
          УВЕЛИЧИТЬ(сколько)
        КОН
      КОН
    ИНАЧЕ
      ОТ сч := 0 ДО Потребителей-1 ВЫП
        ЕСЛИ (План[наКакой, сч] # -1) ИЛИ ((наКакой = начQ_поставщик) И (сч = начQ_потребитель)) ТО
          УВЕЛИЧИТЬ(сколько)
        КОН
      КОН
    КОН;
    ВОЗВРАТ сколько
  КОН НаЛинии;

  ЗАДАЧА^ ИскатьВСтолбце(номер, строка: ЦЕЛ): КЛЮЧ;

  ЗАДАЧА ИскатьВСтроке(номер, столбец: ЦЕЛ): КЛЮЧ;
  ПЕР
    сч: ЦЕЛ;
  УКАЗ
    ЕСЛИ (НЕ начало) И (номер = начQ_поставщик) И (столбец = начQ_потребитель) ТО циклНайден := ВКЛ КОН;
    ЕСЛИ начало ТО начало := ОТКЛ КОН;
    ЕСЛИ циклНайден ТО ВОЗВРАТ ВКЛ КОН;
    ОТ сч := 0 ДО Потребителей-1 ВЫП
      ЕСЛИ
        (сч # столбец) И
        ((План[номер, сч] # -1) ИЛИ ((номер = начQ_поставщик) И (сч = начQ_потребитель))) И
        (НаЛинии(сч, ВКЛ) > 1) И
        (Q[номер, сч] = 0)
      ТО
        Q[номер, сч] := -1;
        ЕСЛИ НЕ ИскатьВСтолбце(сч, номер) ТО Q[номер, сч] := 0 ИНАЧЕ ВОЗВРАТ ВКЛ КОН
      КОН
    КОН;
    ВОЗВРАТ ОТКЛ
  КОН ИскатьВСтроке;

  ЗАДАЧА ИскатьВСтолбце(номер, строка: ЦЕЛ): КЛЮЧ;
  ПЕР
    сч: ЦЕЛ;
  УКАЗ
    ЕСЛИ (НЕ начало) И (строка = начQ_поставщик) И (номер = начQ_потребитель) ТО циклНайден := ВКЛ КОН;
    ЕСЛИ начало ТО начало := ОТКЛ КОН;
    ЕСЛИ циклНайден ТО ВОЗВРАТ ВКЛ КОН;
    ОТ сч := 0 ДО Поставщиков-1 ВЫП
      ЕСЛИ
        (сч # строка) И
        ((План[сч, номер] # -1) ИЛИ ((сч = начQ_поставщик) И (номер = начQ_потребитель))) И
        (НаЛинии(сч, ОТКЛ) > 1) И
        (Q[сч, номер] = 0)
      ТО
        Q[сч, номер] := 1;
        ЕСЛИ НЕ ИскатьВСтроке(сч, номер) ТО Q[сч, номер] := 0 ИНАЧЕ ВОЗВРАТ ВКЛ КОН
      КОН
    КОН;
    ВОЗВРАТ ОТКЛ
  КОН ИскатьВСтолбце;

УКАЗ
  максЗн := 0;
  ОТ сч1 := 0 ДО Потребителей*Поставщиков-(Потребителей+Поставщиков-1)-1 ВЫП
    ЕСЛИ оцСв[сч1].значение > максЗн ТО максЗн := оцСв[сч1].значение КОН
  КОН;
  сч3 := 0;
  ПОКА оцСв[сч3].значение # максЗн ВЫП УВЕЛИЧИТЬ(сч3) КОН;
  ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
    ОТ сч2 := 0 ДО Потребителей-1 ВЫП
      Q[сч1, сч2] := 0
    КОН
  КОН;
  Разница := оцСв[сч3].значение;
  начQ_поставщик := оцСв[сч3].поставщик; начQ_потребитель := оцСв[сч3].потребитель;
  начало := ВКЛ; циклНайден := ОТКЛ;
  ЕСЛИ ИскатьВСтроке(начQ_поставщик, начQ_потребитель) ТО КОН
КОН Цикл;

ЗАДАЧА ИзменитьПлан;
ПЕР
  сч1, сч2: ЦЕЛ;
  занято, недостаток: ЦЕЛ;
УКАЗ
  ЕСЛИ Поправка = 0 ТО
    ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
      ОТ сч2 := 0 ДО Потребителей-1 ВЫП
        ЕСЛИ План[сч1, сч2] = 0 ТО План[сч1, сч2] := -1; сч2 := Потребителей; сч1 := Поставщиков КОН
      КОН
    КОН;
    План[начQ_поставщик, начQ_потребитель] := 0
  ИНАЧЕ
    ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
      ОТ сч2 := 0 ДО Потребителей-1 ВЫП
        ЕСЛИ Q[сч1, сч2] = 1 ТО
          ЕСЛИ План[сч1, сч2] = -1 ТО План[сч1, сч2] := 0 КОН;
          УВЕЛИЧИТЬ(План[сч1, сч2], Поправка);
        АЕСЛИ Q[сч1, сч2] = -1 ТО УМЕНЬШИТЬ(План[сч1, сч2], Поправка)
        КОН
      КОН
    КОН;
    занято := 0;
    ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
      ОТ сч2 := 0 ДО Потребителей-1 ВЫП
        ЕСЛИ План[сч1, сч2] > 0 ТО УВЕЛИЧИТЬ(занято) КОН
      КОН
    КОН;
    ОТ сч1 := 0 ДО Поставщиков-1 ВЫП
      ОТ сч2 := 0 ДО Потребителей-1 ВЫП
        ЕСЛИ План[сч1, сч2] = 0 ТО План[сч1, сч2] := -1 КОН
      КОН
    КОН;
    недостаток := (Поставщиков+Потребителей-1) - занято;
    ЕСЛИ недостаток > 0 ТО РасставитьНули(недостаток) КОН
  КОН
КОН ИзменитьПлан;

УКАЗ
  ПринятьДанные;
  ЗаполнитьОтУгла;
  Разница := -1;
  КОЛЬЦО
    ОценитьБазисныеКлетки;
    ЕСЛИ ОценитьСвободныеКлетки() ТО ВЫХОД КОН;
    Цикл;
    ПосчитатьПоправку;
    ИзменитьПлан
  КОН;
  ВывестиПлан

КОН Транспорт.
```



###  Input 

 Number of suppliers: 3.
 Number of consumers: 3.
 Inventories:
 12 40 33
 Requirements:
 20 30 10
 Introduced a fictitious consumer.
 The matrix of costs:
 3 5 7
 2 4 6
 9 1 8


###  Output 

 --------------------
   -    -    -    12
   20   -    10   10
   -    30   -     3
 --------------------


## Go

```go
package main

import (
    "bufio"
    "container/list"
    "fmt"
    "io/ioutil"
    "log"
    "math"
    "os"
    "strconv"
)

type shipment struct {
    quantity, costPerUnit float64
    r, c                  int
}

var shipZero = shipment{}

type transport struct {
    filename       string
    supply, demand []int
    costs          [][]float64
    matrix         [][]shipment
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func minOf(i, j int) int {
    if i < j {
        return i
    }
    return j
}

func newTransport(filename string) *transport {
    file, err := os.Open(filename)
    check(err)
    defer file.Close()
    scanner := bufio.NewScanner(file)
    scanner.Split(bufio.ScanWords)
    scanner.Scan()
    numSources, err := strconv.Atoi(scanner.Text())
    check(err)
    scanner.Scan()
    numDests, err := strconv.Atoi(scanner.Text())
    check(err)
    src := make([]int, numSources)
    for i := 0; i < numSources; i++ {
        scanner.Scan()
        src[i], err = strconv.Atoi(scanner.Text())
        check(err)
    }
    dst := make([]int, numDests)
    for i := 0; i < numDests; i++ {
        scanner.Scan()
        dst[i], err = strconv.Atoi(scanner.Text())
        check(err)
    }

    // fix imbalance
    totalSrc := 0
    for _, v := range src {
        totalSrc += v
    }
    totalDst := 0
    for _, v := range dst {
        totalDst += v
    }
    diff := totalSrc - totalDst
    if diff > 0 {
        dst = append(dst, diff)
    } else if diff < 0 {
        src = append(src, -diff)
    }

    costs := make([][]float64, len(src))
    for i := 0; i < len(src); i++ {
        costs[i] = make([]float64, len(dst))
    }
    matrix := make([][]shipment, len(src))
    for i := 0; i < len(src); i++ {
        matrix[i] = make([]shipment, len(dst))
    }
    for i := 0; i < numSources; i++ {
        for j := 0; j < numDests; j++ {
            scanner.Scan()
            costs[i][j], err = strconv.ParseFloat(scanner.Text(), 64)
            check(err)
        }
    }
    return &transport{filename, src, dst, costs, matrix}
}

func (t *transport) northWestCornerRule() {
    for r, northwest := 0, 0; r < len(t.supply); r++ {
        for c := northwest; c < len(t.demand); c++ {
            quantity := minOf(t.supply[r], t.demand[c])
            if quantity > 0 {
                t.matrix[r][c] = shipment{float64(quantity), t.costs[r][c], r, c}
                t.supply[r] -= quantity
                t.demand[c] -= quantity
                if t.supply[r] == 0 {
                    northwest = c
                    break
                }
            }
        }
    }
}

func (t *transport) steppingStone() {
    maxReduction := 0.0
    var move []shipment = nil
    leaving := shipZero
    t.fixDegenerateCase()
    for r := 0; r < len(t.supply); r++ {
        for c := 0; c < len(t.demand); c++ {
            if t.matrix[r][c] != shipZero {
                continue
            }
            trial := shipment{0, t.costs[r][c], r, c}
            path := t.getClosedPath(trial)
            reduction := 0.0
            lowestQuantity := float64(math.MaxInt32)
            leavingCandidate := shipZero
            plus := true
            for _, s := range path {
                if plus {
                    reduction += s.costPerUnit
                } else {
                    reduction -= s.costPerUnit
                    if s.quantity < lowestQuantity {
                        leavingCandidate = s
                        lowestQuantity = s.quantity
                    }
                }
                plus = !plus
            }
            if reduction < maxReduction {
                move = path
                leaving = leavingCandidate
                maxReduction = reduction
            }
        }
    }

    if move != nil {
        q := leaving.quantity
        plus := true
        for _, s := range move {
            if plus {
                s.quantity += q
            } else {
                s.quantity -= q
            }
            if s.quantity == 0 {
                t.matrix[s.r][s.c] = shipZero
            } else {
                t.matrix[s.r][s.c] = s
            }
            plus = !plus
        }
        t.steppingStone()
    }
}

func (t *transport) matrixToList() *list.List {
    l := list.New()
    for _, m := range t.matrix {
        for _, s := range m {
            if s != shipZero {
                l.PushBack(s)
            }
        }
    }
    return l
}

func (t *transport) getClosedPath(s shipment) []shipment {
    path := t.matrixToList()
    path.PushFront(s)

    // remove (and keep removing) elements that do not have a
    // vertical AND horizontal neighbor
    var next *list.Element
    for {
        removals := 0
        for e := path.Front(); e != nil; e = next {
            next = e.Next()
            nbrs := t.getNeighbors(e.Value.(shipment), path)
            if nbrs[0] == shipZero || nbrs[1] == shipZero {
                path.Remove(e)
                removals++
            }
        }
        if removals == 0 {
            break
        }
    }

    // place the remaining elements in the correct plus-minus order
    stones := make([]shipment, path.Len())
    prev := s
    for i := 0; i < len(stones); i++ {
        stones[i] = prev
        prev = t.getNeighbors(prev, path)[i%2]
    }
    return stones
}

func (t *transport) getNeighbors(s shipment, lst *list.List) [2]shipment {
    var nbrs [2]shipment
    for e := lst.Front(); e != nil; e = e.Next() {
        o := e.Value.(shipment)
        if o != s {
            if o.r == s.r && nbrs[0] == shipZero {
                nbrs[0] = o
            } else if o.c == s.c && nbrs[1] == shipZero {
                nbrs[1] = o
            }
            if nbrs[0] != shipZero && nbrs[1] != shipZero {
                break
            }
        }
    }
    return nbrs
}

func (t *transport) fixDegenerateCase() {
    eps := math.SmallestNonzeroFloat64
    if len(t.supply)+len(t.demand)-1 != t.matrixToList().Len() {
        for r := 0; r < len(t.supply); r++ {
            for c := 0; c < len(t.demand); c++ {
                if t.matrix[r][c] == shipZero {
                    dummy := shipment{eps, t.costs[r][c], r, c}
                    if len(t.getClosedPath(dummy)) == 0 {
                        t.matrix[r][c] = dummy
                        return
                    }
                }
            }
        }
    }
}

func (t *transport) printResult() {
    fmt.Println(t.filename)
    text, err := ioutil.ReadFile(t.filename)
    check(err)
    fmt.Printf("\n%s\n", string(text))
    fmt.Printf("Optimal solution for %s\n\n", t.filename)
    totalCosts := 0.0
    for r := 0; r < len(t.supply); r++ {
        for c := 0; c < len(t.demand); c++ {
            s := t.matrix[r][c]
            if s != shipZero && s.r == r && s.c == c {
                fmt.Printf(" %3d ", int(s.quantity))
                totalCosts += s.quantity * s.costPerUnit
            } else {
                fmt.Printf("  -  ")
            }
        }
        fmt.Println()
    }
    fmt.Printf("\nTotal costs: %g\n\n", totalCosts)
}

func main() {
    filenames := []string{"input1.txt", "input2.txt", "input3.txt"}
    for _, filename := range filenames {
        t := newTransport(filename)
        t.northWestCornerRule()
        t.steppingStone()
        t.printResult()
    }
}
```


```txt

input1.txt

2 3
25 35
20 30 10
3 5 7
3 2 5

Optimal solution for input1.txt

  20   -     5 
  -    30    5 

Total costs: 180

input2.txt

3 3
12 40 33
20 30 10
3 5 7
2 4 6
9 1 8

Optimal solution for input2.txt

  -    -    -    12 
  20   -    10   10 
  -    30   -     3 

Total costs: 130

input3.txt

4 4
14 10 15 12
10 15 12 15
10 30 25 15
20 15 20 10
10 30 20 20
30 40 35 45

Optimal solution for input3.txt

  -    -    -    14 
  -     9   -     1 
  10   -     5   -  
  -     5    7   -  
  -     1   -    -  

Total costs: 1000

```



## J


The current task description refers to the algorithm by name, but I feel that those names are ambiguous (inadequately descriptive - we need the algorithm specified here on rosettacode, not problems which we expect are so well understood that no one needs to describe them).

So, I will be working with this interpretation:

(*) Assign shipments for the lowest cost unsatisfied need which can be supplied. When breaking ties, pick the first one which would be encountered when scanning left to right, top to bottom (which is the same order you use when reading english text on a page).

(*) Supply however much of the need which can be supplied by that shipment.

(*) Repeat until done.

(If this algorithm is incorrect for this task, that would just underline the need for a better task description. And, probably, also: the need for a more representative task example.)

In other words:


```J
NB. C's y[m] v= x  implemented as  x m ndxasgn v y
ndxasgn=: conjunction define
:
  ((m{y)v x) m} y
)

trans=: adverb define
:
  need=. x
  supl=. y
  cost=. m
  dims=. supl ,&# need
  r=. dims$0
  while. 1 e., xfr=. supl *&*/ need do.
    'iS iN'=. ndxs=. dims#:(i. <./), cost % xfr 
    n=. (iS { supl) <. iN { need
    need=. n iN ndxasgn - need
    supl=. n iS ndxasgn - supl
    r=. n (<ndxs)} r
  end.
)
```


Task data:


```J
need=: 20 30 10
supply=: 25 35
cost=:3 5 7,:3 2 5
```


Task example:


```J
   need cost trans supply
20  0 5
 0 30 5
```



## Java

```java
import java.io.File;
import java.util.*;
import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toCollection;

public class TransportationProblem {

    private static int[] demand;
    private static int[] supply;
    private static double[][] costs;
    private static Shipment[][] matrix;

    private static class Shipment {
        final double costPerUnit;
        final int r, c;
        double quantity;

        public Shipment(double q, double cpu, int r, int c) {
            quantity = q;
            costPerUnit = cpu;
            this.r = r;
            this.c = c;
        }
    }

    static void init(String filename) throws Exception {

        try (Scanner sc = new Scanner(new File(filename))) {
            int numSources = sc.nextInt();
            int numDestinations = sc.nextInt();

            List<Integer> src = new ArrayList<>();
            List<Integer> dst = new ArrayList<>();

            for (int i = 0; i < numSources; i++)
                src.add(sc.nextInt());

            for (int i = 0; i < numDestinations; i++)
                dst.add(sc.nextInt());

            // fix imbalance
            int totalSrc = src.stream().mapToInt(i -> i).sum();
            int totalDst = dst.stream().mapToInt(i -> i).sum();
            if (totalSrc > totalDst)
                dst.add(totalSrc - totalDst);
            else if (totalDst > totalSrc)
                src.add(totalDst - totalSrc);

            supply = src.stream().mapToInt(i -> i).toArray();
            demand = dst.stream().mapToInt(i -> i).toArray();

            costs = new double[supply.length][demand.length];
            matrix = new Shipment[supply.length][demand.length];

            for (int i = 0; i < numSources; i++)
                for (int j = 0; j < numDestinations; j++)
                    costs[i][j] = sc.nextDouble();
        }
    }

    static void northWestCornerRule() {

        for (int r = 0, northwest = 0; r < supply.length; r++)
            for (int c = northwest; c < demand.length; c++) {

                int quantity = Math.min(supply[r], demand[c]);
                if (quantity > 0) {
                    matrix[r][c] = new Shipment(quantity, costs[r][c], r, c);

                    supply[r] -= quantity;
                    demand[c] -= quantity;

                    if (supply[r] == 0) {
                        northwest = c;
                        break;
                    }
                }
            }
    }

    static void steppingStone() {
        double maxReduction = 0;
        Shipment[] move = null;
        Shipment leaving = null;

        fixDegenerateCase();

        for (int r = 0; r < supply.length; r++) {
            for (int c = 0; c < demand.length; c++) {

                if (matrix[r][c] != null)
                    continue;

                Shipment trial = new Shipment(0, costs[r][c], r, c);
                Shipment[] path = getClosedPath(trial);

                double reduction = 0;
                double lowestQuantity = Integer.MAX_VALUE;
                Shipment leavingCandidate = null;

                boolean plus = true;
                for (Shipment s : path) {
                    if (plus) {
                        reduction += s.costPerUnit;
                    } else {
                        reduction -= s.costPerUnit;
                        if (s.quantity < lowestQuantity) {
                            leavingCandidate = s;
                            lowestQuantity = s.quantity;
                        }
                    }
                    plus = !plus;
                }
                if (reduction < maxReduction) {
                    move = path;
                    leaving = leavingCandidate;
                    maxReduction = reduction;
                }
            }
        }

        if (move != null) {
            double q = leaving.quantity;
            boolean plus = true;
            for (Shipment s : move) {
                s.quantity += plus ? q : -q;
                matrix[s.r][s.c] = s.quantity == 0 ? null : s;
                plus = !plus;
            }
            steppingStone();
        }
    }

    static LinkedList<Shipment> matrixToList() {
        return stream(matrix)
                .flatMap(row -> stream(row))
                .filter(s -> s != null)
                .collect(toCollection(LinkedList::new));
    }

    static Shipment[] getClosedPath(Shipment s) {
        LinkedList<Shipment> path = matrixToList();
        path.addFirst(s);

        // remove (and keep removing) elements that do not have a
        // vertical AND horizontal neighbor
        while (path.removeIf(e -> {
            Shipment[] nbrs = getNeighbors(e, path);
            return nbrs[0] == null || nbrs[1] == null;
        }));

        // place the remaining elements in the correct plus-minus order
        Shipment[] stones = path.toArray(new Shipment[path.size()]);
        Shipment prev = s;
        for (int i = 0; i < stones.length; i++) {
            stones[i] = prev;
            prev = getNeighbors(prev, path)[i % 2];
        }
        return stones;
    }

    static Shipment[] getNeighbors(Shipment s, LinkedList<Shipment> lst) {
        Shipment[] nbrs = new Shipment[2];
        for (Shipment o : lst) {
            if (o != s) {
                if (o.r == s.r && nbrs[0] == null)
                    nbrs[0] = o;
                else if (o.c == s.c && nbrs[1] == null)
                    nbrs[1] = o;
                if (nbrs[0] != null && nbrs[1] != null)
                    break;
            }
        }
        return nbrs;
    }

    static void fixDegenerateCase() {
        final double eps = Double.MIN_VALUE;

        if (supply.length + demand.length - 1 != matrixToList().size()) {

            for (int r = 0; r < supply.length; r++)
                for (int c = 0; c < demand.length; c++) {
                    if (matrix[r][c] == null) {
                        Shipment dummy = new Shipment(eps, costs[r][c], r, c);
                        if (getClosedPath(dummy).length == 0) {
                            matrix[r][c] = dummy;
                            return;
                        }
                    }
                }
        }
    }

    static void printResult(String filename) {
        System.out.printf("Optimal solution %s%n%n", filename);
        double totalCosts = 0;

        for (int r = 0; r < supply.length; r++) {
            for (int c = 0; c < demand.length; c++) {

                Shipment s = matrix[r][c];
                if (s != null && s.r == r && s.c == c) {
                    System.out.printf(" %3s ", (int) s.quantity);
                    totalCosts += (s.quantity * s.costPerUnit);
                } else
                    System.out.printf("  -  ");
            }
            System.out.println();
        }
        System.out.printf("%nTotal costs: %s%n%n", totalCosts);
    }

    public static void main(String[] args) throws Exception {

        for (String filename : new String[]{"input1.txt", "input2.txt",
            "input3.txt"}) {
            init(filename);
            northWestCornerRule();
            steppingStone();
            printResult(filename);
        }
    }
}
```



```txt
input1.txt

2 3
25 35
20 30 10
3 5 7
3 2 5

Optimal solution input1.txt

  20   -     5 
  -    30    5 

Total costs: 180.0

```



```txt
input2.txt

3 3
12 40 33
20 30 10
3 5 7
2 4 6
9 1 8

Optimal solution input2.txt

  -    -    -    12 
  20   -    10   10 
  -    30   -     3 

Total costs: 130.0

```



```txt
input3.txt

4 4
14 10 15 12
10 15 12 15
10 30 25 15
20 15 20 10
10 30 20 20
30 40 35 45

Optimal solution input3.txt

  -    -    -    14 
  -     9   -     1 
  10   -     5   -  
  -     5    7   -  
  -     1   -    -  

Total costs: 1000.0
```



## Julia

Using [https://github.com/JuliaOpt/MathProgBase.jl MathProgBase.jl].

```julia
using MathProgBase, Clp

c = [3, 5, 7, 3, 2, 5]

A = [1 1 1 0 0 0
     0 0 0 1 1 1
     1 0 0 1 0 0
     0 1 0 0 1 0
     0 0 1 0 0 1]

b = [ 25,  35,  20,  30,  10]
s = ['<', '<', '=', '=', '=']

sol = linprog(c, A, b, s, ClpSolver())
@show sol.status
@show sol.sol
@show sol.objval
```


```txt
sol.status = :Optimal
sol.sol = [20.0, 0.0, 5.0, 0.0, 30.0, 5.0]
sol.objval = 180.0
```



## Kotlin

```scala
// version 1.1.51

import java.io.File
import java.util.Scanner
import java.util.LinkedList

class Transport(val filename: String) {

    private val supply: IntArray
    private val demand: IntArray
    private val costs : Array<DoubleArray>
    private val matrix: Array<Array<Shipment>>

    class Shipment(
        var quantity: Double,
        val costPerUnit: Double,
        val r: Int,
        val c: Int
    )

    companion object {
        private val ZERO = Shipment(0.0, 0.0, -1, -1) // to avoid nullable Shipments
    }

    init {
        val sc = Scanner(File(filename))
        try {
            val numSources = sc.nextInt()
            val numDestinations = sc.nextInt()
            val src = MutableList(numSources) { sc.nextInt() }
            val dst = MutableList(numDestinations) { sc.nextInt() }

            // fix imbalance
            val totalSrc = src.sum()
            val totalDst = dst.sum()
            if (totalSrc > totalDst)
                dst.add(totalSrc -totalDst)
            else if (totalDst > totalSrc)
                src.add(totalDst -totalSrc)
            supply = src.toIntArray()
            demand = dst.toIntArray()

            costs  = Array(supply.size) { DoubleArray(demand.size) } 
            matrix = Array(supply.size) { Array(demand.size) { ZERO } }
            for (i in 0 until numSources) {
                for (j in 0 until numDestinations) costs[i][j] = sc.nextDouble()
            }            
        }
        finally {
            sc.close()
        }
    }

    fun northWestCornerRule() {
        var northwest = 0
        for (r in 0 until supply.size) {
            for (c in northwest until demand.size) {
                val quantity = minOf(supply[r], demand[c]).toDouble()
                if (quantity > 0.0) {
                    matrix[r][c] = Shipment(quantity, costs[r][c], r, c)
                    supply[r] -= quantity.toInt()
                    demand[c] -= quantity.toInt()
                    if (supply[r] == 0) {
                        northwest = c
                        break
                    }
                }
            }
        }
    }

    fun steppingStone() {
        var maxReduction = 0.0
        var move: Array<Shipment>? = null
        var leaving = ZERO
        fixDegenerateCase()

        for (r in 0 until supply.size) {
            for (c in 0 until demand.size) {
                if (matrix[r][c] != ZERO) continue
                val trial = Shipment(0.0, costs[r][c], r, c)
                val path = getClosedPath(trial)
                var reduction = 0.0
                var lowestQuantity = Int.MAX_VALUE.toDouble()
                var leavingCandidate = ZERO
                var plus = true
                for (s in path) {
                    if (plus) {
                        reduction += s.costPerUnit
                    }
                    else {
                        reduction -= s.costPerUnit
                        if (s.quantity < lowestQuantity) {
                            leavingCandidate = s
                            lowestQuantity = s.quantity
                        }
                    }
                    plus = !plus
                }
                if (reduction < maxReduction) {
                    move = path
                    leaving = leavingCandidate
                    maxReduction = reduction
                }
            }
        }

        if (move != null) {
            val q = leaving.quantity
            var plus = true
            for (s in move) {
                s.quantity += if (plus) q else -q
                matrix[s.r][s.c] = if (s.quantity == 0.0) ZERO else s
                plus = !plus
            }
            steppingStone()
        }
    }

    private fun matrixToList() =
        LinkedList<Shipment>(matrix.flatten().filter { it != ZERO } )

    private fun getClosedPath(s: Shipment): Array<Shipment> {
        val path = matrixToList()
        path.addFirst(s)

        // remove (and keep removing) elements that do not have a
        // vertical AND horizontal neighbor
        while (path.removeIf {
            val nbrs = getNeighbors(it, path)
            nbrs[0] == ZERO || nbrs[1] == ZERO
        }) ; // empty statement

        // place the remaining elements in the correct plus-minus order
        val stones = Array<Shipment>(path.size) { ZERO }
        var prev = s
        for (i in 0 until stones.size) {
            stones[i] = prev
            prev = getNeighbors(prev, path)[i % 2]
        }
        return stones
    }

    private fun getNeighbors(s: Shipment, lst: LinkedList<Shipment>): Array<Shipment> {
        val nbrs = Array<Shipment>(2) { ZERO }
        for (o in lst) {
            if (o != s) {
                if (o.r == s.r && nbrs[0] == ZERO)
                    nbrs[0] = o
                else if (o.c == s.c && nbrs[1] == ZERO)
                    nbrs[1] = o
                if (nbrs[0] != ZERO && nbrs[1] != ZERO) break
            }
        }
        return nbrs
    }

    private fun fixDegenerateCase() {
        val eps = Double.MIN_VALUE
        if (supply.size + demand.size - 1 != matrixToList().size) {
            for (r in 0 until supply.size) {
                for (c in 0 until demand.size) {
                    if (matrix[r][c] == ZERO) {
                        val dummy = Shipment(eps, costs[r][c], r, c)
                        if (getClosedPath(dummy).size == 0) {
                            matrix[r][c] = dummy
                            return
                        }
                    }
                }
            }
        }
    }

    fun printResult() {
        val text = File(filename).readText()
        println("$filename\n\n$text")
        println("Optimal solution $filename\n")
        var totalCosts = 0.0
 
        for (r in 0 until supply.size) {
            for (c in 0 until demand.size) {
                val s = matrix[r][c]
                if (s != ZERO && s.r == r && s.c == c) {
                    print(" %3s ".format(s.quantity.toInt()))
                    totalCosts += s.quantity * s.costPerUnit
                }
                else print("  -  ")
            }
            println()
        }
        println("\nTotal costs: $totalCosts\n")
    }
}

fun main(args: Array<String>) {
    val filenames = arrayOf("input1.txt", "input2.txt", "input3.txt")
    for (filename in filenames) {
        with (Transport(filename)) {
            northWestCornerRule()
            steppingStone()
            printResult()
        }
    }
}
```


```txt

Same as Java entry

```



## Pascal


```pascal
Program transport;
{ based on the program of <Svetlana Belashova> }

Uses Crt;

Label l1;

Const N=10;
      n1=7; n2=7;
      Sa:longint=0;
      Sb:longint=0;

Type points=Array [1..N] of longint;
     distribution=Array [1..N,1..N] of longint;

Var A,B,alfa,beta,B_d,x:points;
    c,p:distribution;
    f,f0,x_min,Sp:longint;
    Nt,x_p,r,r_min,ki,kj,Na,Nb,h,l,i,j:byte;
    d:char;
    u:Array[1..N*N] of byte;

Procedure Nul (var a:points);
var i:byte;
Begin
     for i:=1 to N do a[i]:=0;
End;

Procedure PrintS (x,y:byte; s:string; c:byte);
Begin
     TextColor(c);
     GotoXY(x,y);
     Write(s);
End;

Procedure Print (x,y:byte; n:byte; a:longint; c:byte);
Begin
     TextColor(c);
     GotoXY(x,y); Write(' ':n);
     GotoXY(x,y); Write(a);
End;

Procedure Read (var x:longint; y:byte);
var i:integer;
    s:string;
    c:char;
    j,k:byte;
Begin
     s:=''; i:=1;
     TextColor(11);
     Repeat
           c:=ReadKey;
           Case ord(c) of
48..57:         begin s:=s+c;
                      Write(c);
                      inc(i);
                end;
8:              if i>1 then begin dec(i);
                      Delete(s,i,1);
                      Write(chr(8),' ',chr(8));
                end;
           end;
           j:=WhereX;
           GotoXY(60,1); ClrEOL;
           if i>y then begin
              TextColor(4);
              Write('Not more than ');
              for k:=1 to y-1 do Write('9');
              TextColor(11);
           end;
           GotoXY(j,1);
     Until (ord(c)=13) and (i<y+1);
     val(s,x,i);
End;

Procedure horizontal (a,b,c,d,e:char);
var i,j:byte;
Begin
     Write(a);
     for i:=1 to n2 do Write(b);
     Write(c);
     for i:=1 to Nb do begin
         for j:=1 to n1 do Write(b);
         if i<>Nb then Write(d) else Write(c);
     end;
     for i:=1 to 4 do Write(b);
     Write(e);
End;

Procedure vertical;
var i:byte;
Begin
     Write('│',' ':n2,'║');
     for i:=1 to Nb-1 do Write(' ':n1,'│');
     WriteLn(' ':n1,'║',' ' :4,'│');
End;

Procedure Table; { Drawing the table }
Begin
    ClrScr;
    TextColor(1);
    h:=6+Na*3;
    l:=14+Nb*7;
    GotoXY(1,3);
    for i:=3 to h do vertical;
    GotoXY(1,2);
    horizontal('┌','─','╥','┬','┐');
    for i:=1 to Na+1 do begin
        GotoXY(1,i*3+2);
        if (i=1) or (i=Na+1)
           then horizontal('╞','═','╬','╪','╡')
           else horizontal('├','─','╫','┼','┤');
    end;
    GotoXY(1,h+1);
    horizontal('└','─','╨','┴','┘');
    TextColor(9);
    for i:=1 to Na do begin
        GotoXY(5,i*3+3);
        Write('A',i);
    end;
    for i:=1 to Nb do begin
        GotoXY(i*(n1+1)+n2-2,3);
        Write('B',i);
    end;
    l:=Nb*(n1+1)+n2+3;
    h:=Na*3+6;
    PrintS(4,3,'\Bj',9);
    PrintS(4,4,'Ai\',9);
    PrintS(1,1,'Table N1',14);
    PrintS(l,4,'alfa',9);
    PrintS(3,h,'beta',9);
End;

Procedure EnterIntoTheTable (var a:points; b:byte; c:char); { Entering into the table }
var i,l,m:byte;
Begin
     for i:=1 to b do begin
         TextColor(3);
         GotoXY(32,1);
         ClrEOL;
         Write(c,i,'=  ');
         Read(a[i],n1);
         TextColor(14);
         Case c of
'A':     GotoXY(n2-trunc(ln(a[i])/ln(10)),i*3+4);
'B':     GotoXY(n2+i*(n1+1)-trunc(ln(a[i])/ln(10)),4);
         end;
         Write(a[i]);
     end;
End;

Function CalculatingTheCost:longint;        { Calculating the cost of the plan }
var i,j:byte;
    f:longint;
Begin
     f:=0;
     for i:=1 to Na do
         for j:=1 to Nb do
             if p[i,j]>0 then inc(f,c[i,j]*p[i,j]);
     GotoXY(65,Nt+2);
     TextColor(10);
     Write('F',Nt,'=',f);
     CalculatingTheCost:=f;
End;

Function CalculatingThePotentials:boolean;      { Calculating the potentials }
var k,i,j:byte;
    Z_a,Z_b:points;
    d:boolean;
Begin
     Nul(Z_a); Nul(Z_b);
     alfa[1]:=0; Z_a[1]:=1; k:=1;
     Repeat
           d:=1=1;
           for i:=1 to Na do
               if Z_a[i]=1 then
                  for j:=1 to Nb do
                      if (p[i,j]>-1) and (Z_b[j]=0) then begin
                         Z_b[j]:=1;
                         beta[j]:=c[i,j]-alfa[i];
                         inc(k);
                         d:=1=2;
                      end;
           for i:=1 to Nb do
               if Z_b[i]=1 then
                  for j:=1 to Na do
                      if (p[j,i]>-1) and (Z_a[j]=0) then begin
                         Z_a[j]:=1;
                         alfa[j]:=c[j,i]-beta[i];
                         inc(k);
                         d:=1=2;
                      end;
     Until (k=Na+Nb) or d;
     if d then begin
        i:=1;
        While Z_a[i]=1 do inc(i);
        j:=1;
        While Z_b[j]=0 do inc(j);
        p[i,j]:=0;
        Print((j+1)*(n1+1)+n2-8,i*3+4,1,p[i,j],7);
     end;

     CalculatingThePotentials:=d;
End;

Procedure OutputThePlan;         { Output the plan of distribution }
var i,j,h,l,k:byte;
    c_max:longint;
Begin
     k:=0;
     for i:=1 to Na do begin
         h:=i*3+4;
         for j:=1 to Nb do begin
             l:=j*(n1+1)+n2-5;
             GotoXY(l,h);
             Write(' ':n1);
             if p[i,j]>0 then begin
                inc(k);
                Print(l-trunc(ln(p[i,j])/ln(10))+5,h,1,p[i,j],14);
             end
             else if p[i,j]=0 then begin
                     Print(l+n1-2,h,1,p[i,j],14);
                     inc(k);
             end;
         end;
     end;

     While CalculatingThePotentials do inc(k);

     if k>Na+Nb-1 then PrintS(40,1,'k > n+m-1',12);
End;

Function CalculatingTheCoefficients(var ki,kj:byte):integer; { Calculation the coefficients in the free cells }
var i,j:byte;
    k,k_min:integer;
    b:boolean;
Begin
     b:=1=1;
     for i:=1 to Na do
         for j:=1 to Nb do
             if p[i,j]=-1 then begin
                k:=c[i,j]-alfa[i]-beta[j];
                if b then begin
                   b:=1=2;
                   ki:=i; kj:=j; k_min:=k;
                end else
                    if k<k_min then begin
                       k_min:=k;
                       ki:=i; kj:=j;
                    end;
                TextColor(6);
                GotoXY(j*(n1+1)+n2-5,i*3+4);
                Write('(',k,')');
             end;
     if k_min<0 then PrintS(kj*(n1+1)+n2,ki*3+4,'X',12);
     CalculatingTheCoefficients:=k_min;
End;

Procedure div_mod(c:byte; var a,b:byte);   { Translate one-dimensional array to two-dimensional }
Begin
     b:=c mod Nb; a:=c div Nb +1;
     if b=0 then begin
        b:=Nb; dec(a);
     end;
End;

Procedure Recursive(Xi,Yi:byte; var z:boolean; var c:byte);
var i,j:byte;
Begin
   z:=1=2;
   Case c of
1:   for i:=1 to Na do
         if i<>Xi then
            if p[i,Yi]>-1 then begin
               if u[(i-1)*Nb+Yi]=0 then begin
                  u[(Xi-1)*Nb+Yi]:=(i-1)*Nb+Yi;
                  c:=2;
                  Recursive(i,Yi,z,c);
                  if z then exit;
               end;
            end
            else if (i=ki) and (Yi=kj) then begin
                    u[(Xi-1)*Nb+Yi]:=(ki-1)*Nb+kj;
                    z:=not z;
                    exit;
            end;
2:   for i:=1 to Nb do
         if i<>Yi then
            if p[Xi,i]>-1 then begin
               if u[(Xi-1)*Nb+i]=0 then begin
                  u[(Xi-1)*Nb+Yi]:=(Xi-1)*Nb+i;
                  c:=1;
                  Recursive(Xi,i,z,c);
                  if z then exit;
               end;
            end
            else if (Xi=ki) and (i=kj) then begin
                    u[(Xi-1)*Nb+Yi]:=(ki-1)*Nb+kj;
                    z:=not z;
                    exit;
            end;
   end;
   u[(Xi-1)*Nb+Yi]:=0;
   c:=c mod 2 +1;
End;

Procedure Contour;       { Determine the contour of displacement }
var i,j,k,mi,mj,l:byte;
    z:boolean;
    p_m:longint;
Begin
     for i:=1 to N*N do u[i]:=0;
     l:=1;
     Recursive(ki,kj,z,l);
     i:=ki; j:=kj;
     k:=u[(i-1)*Nb+j];
     div_mod(k,i,j);
     mi:=i; mj:=j; l:=1;
     Repeat
           inc(l);
           k:=u[(i-1)*Nb+j];
           div_mod(k,i,j);
           if l mod 2=1 then
              if p[i,j]<p[mi,mj] then begin
                 mi:=i; mj:=j;
              end;
     Until (i=ki) and (j=kj);

     i:=ki; j:=kj; l:=0;
     p_m:=p[mi,mj];
     Repeat
           if l mod 2=0 then begin
              inc(p[i,j],p_m);
              PrintS((n1+1)*j+n2-1,i*3+3,'(+)',12);
           end else begin
               dec(p[i,j],p_m);
               PrintS((n1+1)*j+n2-1,i*3+3,'(-)',12);
           end;
           if l=0 then inc(p[i,j]);
           k:=u[(i-1)*Nb+j];
           div_mod(k,i,j);
           inc(l);
     Until (i=ki) and (j=kj);
     p[mi,mj]:=-1;
End;

Procedure Pause;
var d:char;
Begin
     TextColor(6);
     GotoXY(40,1);
     Write('Press any key');
     d:=ReadKey;
     GotoXY(40,1);
     ClrEOL;
End;

BEGIN
    Nul(alfa); Nul(beta);
    Nt:=1;
    ClrScr;
    TextColor(10);
    Repeat
          Write('Enter the number of suppliers (2<=Na<=',N-1,')   ');
          ReadLn(Na);
          Write('Enter the number of consumers (2<=Nb<=',N-1,')   ');
          ReadLn(Nb);
    Until (Na>1) and (Na<=N-1) and (Nb>1) and (Nb<=N-1);
    Table;

    PrintS(1,1,'Enter the production quantity:',3);
    EnterIntoTheTable(A,Na,'A');
    EnterIntoTheTable(B,Nb,'B');
    TextColor(3);
    GotoXY(1,1); ClrEOL;
    Write('Enter the cost of transportation');
    for i:=1 to Na do
        for j:=1 to Nb do begin
            TextColor(3);
            GotoXY(29,1); ClrEOL;
            Write('A',i,' - B',j,'  ');
            Read(c[i,j],5);
            Print((n1+1)*j+n2-4,i*3+3,1,c[i,j],11);
        end;

    GotoXY(1,1);
    ClrEOL;
    TextColor(14);
    Write('Table N1');

    for i:=1 to Na do Sa:=Sa+A[i];
    for i:=1 to Nb do Sb:=Sb+B[i];
    if Sa<>Sb then begin
       PrintS(20,1,'The problem is open (Press any key)',7);
       d:=ReadKey;
       if Sa>Sb then begin
          inc(Nb);
          B[Nb]:=Sa-Sb;
          for i:=1 to Na do c[i,Nb]:=0;
       end else begin
           inc(Na);
           A[Na]:=Sb-Sa;
           for i:=1 to Nb do c[Na,i]:=0;
       end;
       Table;
       for i:=1 to Na do
           for j:=1 to Nb do Print((n1+1)*j+n2-4,i*3+3,1,c[i,j],11);
       for i:=1 to Na do
           Print(n2-trunc(ln(A[i])/ln(10)),i*3+4,1,A[i],14);
       for i:=1 to Nb do
           Print(n2+i*(n1+1)-trunc(ln(B[i])/ln(10)),4,1,B[i],14);
       PrintS(20,1,'The problem is open',7);
    end
       else PrintS(20,1,'The problem is closed',7);

(**************** Drafting the basic plan ******************)
    for i:=1 to Nb do B_d[i]:=B[i];
    for i:=1 to Na do begin
        for j:=1 to Nb do x[j]:=j;
        for j:=1 to Nb-1 do begin
            x_min:=c[i,x[j]];
            r_min:=j;
            for r:= j+1 to Nb do
                if (x_min>c[i,x[r]]) or
                 ((x_min=c[i,x[r]]) and (B[x[r]]>b[x[r_min]])) then
                begin
                   x_min :=c[i,x[r]];
                   r_min:=r;
                end;
            x_p:=x[r_min];
            x[r_min]:=x[j];
            x[j]:=x_p;
        end;
        Sp:=0;
        for j:=1 to Nb do begin
            p[i,x[j]]:=B_d[x[j]];
            if p[i,x[j]]>A[i]-Sp then p[i,x[j]]:=A[i]-Sp;
            inc(Sp,p[i,x[j]]);
            dec(B_d[x[j]],p[i,x[j]]);
        end;
    end;
(***********************************************************)

    for i:=1 to Na do
        for j:=1 to Nb do if p[i,j]=0 then p[i,j]:=-1;
    OutputThePlan;
    f:=CalculatingTheCost; f0:=F;

    While CalculatingThePotentials do;
    for i:=1 to Na do Print(l+1,i*3+3,3,alfa[i],11);
    for i:=1 to Nb do Print(i*(n1+1)+n2-4,h,6,beta[i],11);
    Pause;

(******* gradual approach the plan to the optimality ******)
    While CalculatingTheCoefficients(ki,kj)<0 do begin
          Contour;
          pause;
          for i:=1 to Na do
              for j:=1 to Nb do PrintS((n1+1)*j+n2-1,i*3+3,'   ',14);
          inc(Nt);
          GotoXY(1,1);
          Write('Table N',Nt);
          OutputThePlan;
          f0:=f; f:=CalculatingTheCost;
          if CalculatingThePotentials then Goto l1;
          for i:=1 to Na do Print(l+1,i*3+3,3,alfa[i],11);
          for i:=1 to Nb do Print(i*(n1+1)+n2-4,h,6,beta[i],11);
          Pause;
    end;
(***********************************************************)

    PrintS(40,1,'Solution is optimal',12);
    PrintS(60,1,'(any key)',6);
    for i:=1 to Na do
        for j:=1 to Nb do if p[i,j]=-1 then begin
            h:=i*3+4;
            l:=j*(n1+1)+n2-5;
            GotoXY(l,h);
            Write(' ':n1);
        end;
    GotoXY(40,1);
l1: d:=ReadKey;
END.
```



## Perl 6

Using [[Vogel's_approximation_method#Perl6|Vogel's approximation method]]:

```perl6
my  %costs = :S1{:3C1, :5C2, :7C3}, :S2{:3C1, :2C2, :5C3};
my %demand = :20C1, :30C2, :10C3;
my %supply = :25S1, :35S2;

my @cols = %demand.keys.sort;

my %res;
my %g = (|%supply.keys.map: -> $x { $x => [%costs{$x}.sort(*.value)».key]}),
   (|%demand.keys.map: -> $x { $x => [%costs.keys.sort({%costs{$_}{$x}})]});

while (+%g) {
    my @d = %demand.keys.map: -> $x
      {[$x, my $z = %costs{%g{$x}[0]}{$x},%g{$x}[1] ?? %costs{%g{$x}[1]}{$x} - $z !! $z]}

    my @s = %supply.keys.map: -> $x
      {[$x, my $z = %costs{$x}{%g{$x}[0]},%g{$x}[1] ?? %costs{$x}{%g{$x}[1]} - $z !! $z]}

    @d = |@d.grep({ (.[2] == max @d».[2]) }).&min: :by(*.[1]);
    @s = |@s.grep({ (.[2] == max @s».[2]) }).&min: :by(*.[1]);

    my ($t, $f) = @d[2] == @s[2] ?? (@s[1],@d[1]) !! (@d[2],@s[2]);
    my ($d, $s) = $t > $f ?? (@d[0],%g{@d[0]}[0]) !! (%g{@s[0]}[0], @s[0]);

    my $v = %supply{$s} min %demand{$d};

    %res{$s}{$d} += $v;
    %demand{$d} -= $v;

    if (%demand{$d} == 0) {
        %supply.grep( *.value != 0 )».key.map: -> $v
          { %g{$v}.splice((%g{$v}.first: * eq $d, :k),1) };
        %g{$d}:delete;
        %demand{$d}:delete;
    }

    %supply{$s} -= $v;

    if (%supply{$s} == 0) {
        %demand.grep( *.value != 0 )».key.map: -> $v
          { %g{$v}.splice((%g{$v}.first: * eq $s, :k),1) };
        %g{$s}:delete;
        %supply{$s}:delete;
    }
}

say join "\t", flat '', @cols;
my $total;
for %costs.keys.sort -> $g {
    print "$g\t";
    for @cols -> $col {
        print %res{$g}{$col} // '-', "\t";
        $total += (%res{$g}{$col} // 0) * %costs{$g}{$col};
    }
    print "\n";
}
say "\nTotal cost: $total";
```

```txt
	C1	C2	C3
S1	20	-	5	
S2	-	30	5	
```



## Phix

The simplest solution I could think of.

Assumes 0 cost is not allowed, but using say -1 as the "done" cost instead should be fine.

```Phix
procedure solve(sequence needs, avail, costs)
    sequence res = repeat(repeat(0,length(needs)),length(avail))
    while true do
        integer best = 0, supplier, customer
        for s=1 to length(costs) do
            for c=1 to length(costs[s]) do
                integer csc = costs[s][c]
                if csc!=0 and (best=0 or csc<best) then
                    best = csc
                    supplier = s
                    customer = c
                end if
            end for
        end for
        if best=0 then exit end if -- all costs examined
        integer amt = min(avail[supplier],needs[customer])
        -- obviously amt can be 0, in which case this just
        -- removes cost entry from further consideration.
        avail[supplier] -= amt
        needs[customer] -= amt
        res[supplier,customer] = amt
        costs[supplier,customer] = 0
    end while
    pp(res,{pp_Nest,1})             
end procedure

constant needs = {20,30,10},    -- (customers)
         avail = {25,35},       -- (suppliers)
         costs = {{3,5,7},      -- (length suppliers rows of
                  {3,2,5}}      --  length customers columns)

solve(needs,avail,costs)
```

```txt

{{20,0,5},
 {0,30,5}}

```


### stepping stones

Obviously I did not really quite understand the problem when I rattled out the above... this does much better.
```Phix
-- demo\rosetta\Transportation_problem.exw
enum QTY, COST, R, C -- (a shipment)
constant eps = 1e-12
 
function print_matrix(sequence matrix)
    atom total_costs = 0.0
    for r=1 to length(matrix) do
        for c=1 to length(matrix[r]) do
            object s = matrix[r][c]
            string st = "  -  "
            if s!=0 and s[R]==r and s[C]==c then
                atom qty = round(s[QTY]) -- (remove +/-eps)
                if qty!=0 then
                    st = sprintf(" %3d ", qty)
                    total_costs += qty * s[COST]
                end if
            end if
            puts(1,st)
        end for
        printf(1,"\n")
    end for
    return total_costs
end function

procedure print_result(sequence transport, atom expected)
    sequence matrix = transport[4]
    printf(1,"Optimal solution\n\n")
    atom total_costs = print_matrix(matrix)
    printf(1,"\nTotal costs: %g (expected %g)\n\n", {total_costs,expected})
end procedure

function get_neighbors(sequence shipment, lst)
    sequence nbrs = {0,0}
    for e=1 to length(lst) do
        sequence o = lst[e]
        if o!=shipment then
            if o[R]==shipment[R] and nbrs[1]==0 then
                nbrs[1] = o
            elsif o[C]==shipment[C] and nbrs[2]==0 then
                nbrs[2] = o
            end if
            if nbrs[1]!=0 and nbrs[2]!=0 then
                exit
            end if
        end if
    end for
    return nbrs
end function

function matrix_to_list(sequence matrix)
    sequence l = {}
    for r=1 to length(matrix) do
        for c=1 to length(matrix[r]) do
            if matrix[r,c]!=0 then
                l = append(l,matrix[r,c])
            end if
        end for
    end for 
    return l
end function

function get_closed_path(sequence matrix, shipment)
    sequence path = matrix_to_list(matrix)
    path = prepend(path,shipment)

    -- remove (and keep removing) elements that do not have a
    -- vertical AND horizontal neighbor
    while true do
        integer removals = 0
        for e=length(path) to 1 by -1 do
            sequence nbrs = get_neighbors(path[e], path)
            if nbrs[1]==0 or nbrs[2]==0 then
                path[e..e] = {}
                removals += 1
            end if
        end for
        if removals==0 then exit end if
    end while
 
    -- place the remaining elements in the correct plus-minus order
    sequence stones = repeat(0,length(path)),
             prev = shipment
    for i=1 to length(stones) do
        stones[i] = prev
        prev = get_neighbors(prev, path)[mod(i,2)+1]
    end for
    return stones
end function
 
function fix_degenerate_case(sequence matrix, costs)
    if length(matrix)+length(matrix[1])-1 != length(matrix_to_list(matrix)) then
        printf(1,"fixing degenerate case...\n")
        for r=1 to length(matrix) do
            for c=1 to length(matrix[r]) do
                if matrix[r][c] == 0 then
                    sequence dummy = {eps, costs[r][c], r, c}
                    if length(get_closed_path(matrix,dummy)) == 0 then
                        matrix[r][c] = dummy
                        return matrix
                    end if
                end if
            end for
        end for
        ?9/0 -- ??
    end if
    return matrix
end function

function initialise(sequence tests, integer t)
    sequence {src,dst,costs} = tests[t]
    string cs = ppf(costs,{pp_Nest,1,pp_StrFmt,1,pp_Indent,7})
    printf(1,"test %d:\nsrc: %v,\ndst: %v,\ncosts: %s\n",{t,src,dst,cs})
 
    -- check for and fix any imbalance
    atom totalSrc = sum(src),
         totalDst = sum(dst),
         diff = totalSrc-totalDst
    if diff>0 then
        puts(1,"adding dummy consumer...\n")
        dst = append(dst, diff)
        for i=1 to length(costs) do
            costs[i] &= 0
        end for
    elsif diff<0 then
        puts(1,"adding dummy supplier...\n")
        src = append(src, -diff)
        costs = append(costs,repeat(0,length(dst)))
    end if
 
    printf(1,"generating initial feasible solution using northwest corner method...\n")
    sequence matrix = repeat(repeat(0,length(dst)),length(src))
    integer northwest = 1
    for r=1 to length(src) do
        for c=northwest to length(dst) do
            atom qty = min(src[r],dst[c])
            if qty>0 then
                matrix[r][c] = {qty,costs[r,c],r,c}
                src[r] -= qty
                dst[c] -= qty
                if src[r]=0 then
                    northwest = c
                    exit
                end if
            end if
        end for
    end for
    printf(1,"\nTotal costs: %g\n\n", print_matrix(matrix))
    
    return {src,dst,costs,matrix}
end function
 
function stepping_stone(sequence transport)
    sequence {src, dst, costs, matrix} = transport
    atom maxReduction = 0
    object move = NULL, leaving
    matrix = fix_degenerate_case(matrix, costs)
    for r=1 to length(src) do
        for c=1 to length(dst) do
            if matrix[r][c] = 0 then
                sequence trial_shipment = {0, costs[r][c], r, c},
                         path = get_closed_path(matrix,trial_shipment)
                atom reduction = 0.0,
                     lowestQuantity = 1e308
                object leavingCandidate = 0
                bool plus = true
                for i=1 to length(path) do
                    sequence s = path[i]
                    if plus then
                        reduction += s[COST]
                    else
                        reduction -= s[COST]
                        if s[QTY] < lowestQuantity then
                            leavingCandidate = s
                            lowestQuantity = s[QTY]
                        end if
                    end if
                    plus = not plus
                end for
                if reduction < maxReduction then
                    move = path
                    leaving = leavingCandidate
                    maxReduction = reduction
                end if
            end if
        end for
    end for
 
    if move!=NULL then
        atom q = leaving[QTY]
        bool plus = true
        for i=1 to length(move) do
            sequence s = move[i]
            if plus then
                s[QTY] += q
            else
                s[QTY] -= q
            end if
            if s[QTY] == 0 then
                matrix[s[R]][s[C]] = 0
            else
                matrix[s[R]][s[C]] = s
            end if
            plus = not plus
        end for
        {src, dst, costs, matrix} = stepping_stone({src, dst, costs, matrix})
    end if
    return {src, dst, costs, matrix}
end function
 
constant tests = {{{25,35},         -- src
                   {20,30,10},      -- dst
                   {{3,5,7},        -- costs[si][di]
                    {3,2,5}},
                   180},            -- expected total
                  {{12,40,33},      -- src
                   {20,30,10},      -- dst
                   {{3,5,7},        -- costs[si][di]
                    {2,4,6},
                    {9,1,8}},
                   130},            -- expected total
                  {{14,10,15,12},   -- src
                   {10,15,12,15},   -- dst
                   {{10,30,25,15},  -- costs[si][di]
                    {20,15,20,10},
                    {10,30,20,20},
                    {30,40,35,45}},
                   1000},           -- expected total
                  {{100,300,300},   -- src
                   {300,200,200},   -- dst
                   {{50,40,30},     -- costs[si][di]
                    {80,40,30},
                    {90,70,50}},
                   39000},          -- expected total
                  {{40,60,50},      -- src
                   {20,30,50,50},   -- dst
                   {{4,6,8,8},      -- costs[si][di]
                    {6,8,6,7},
                    {5,7,6,8}},
                   920},            -- expected total
                  {{12,1,5},        -- src
                   {10,8},          -- dst
                   {{2,4},          -- costs[si][di]
                    {8,12},
                    {12,6}},
                   68},             -- expected total
                  {{7,9,18},        -- src
                   {5,8,7,14},  -- dst
                   {{19,30,50,10},  -- costs[si][di]
                    {70,30,40,60},
                    {40 ,8,70,20}},
                   743},            -- expected total
                  {{12,11,14,8},    -- src
                   {10,11,15,5,4},  -- dst
                   {{7,12,1,5,6},   -- costs[si][di]
                    {15,3,12,6,14},
                    {8,16,10,12,7},
                    {18,8,17,11,16}},
                   259},            -- expected total
                  {{50,75,25},      -- src
                   {20,20,50,60},   -- dst
                   {{3,5,7,6},      -- costs[si][di]
                    {2,5,8,2},
                    {3,6,9,2}},
                   610}}            -- expected total

--for i=1 to length(tests) do
for i=3 to 3 do
    print_result(stepping_stone(initialise(tests,i)),tests[i][4])
end for
```

(Obviously the other eight tests all work fine and produce similar output.)

```txt

test 3:
src: {14,10,15,12},
dst: {10,15,12,15},
costs: {{10,30,25,15},
        {20,15,20,10},
        {10,30,20,20},
        {30,40,35,45}}

adding dummy supplier...
generating initial feasible solution using northwest corner method...
  10    4   -    -
  -    10   -    -
  -     1   12    2
  -    -    -    12
  -    -    -     1

Total costs: 1220

fixing degenerate case...
Optimal solution

  -    -    -    14
  -     9   -     1
  10   -     5   -
  -     5    7   -
  -     1   -    -

Total costs: 1000 (expected 1000)

```

Note that [[Vogel%27s_approximation_method#Phix]] gets a few of the others wrong and loops on #2, then again that is unbalanced (needs a dummy customer), and I'm not sure whether throwing such at VAM is fair or not.


## Racket

{{trans|Java}} (I understand the letters in Java!)

Using <code>typed/racket</code>, to keep track of Vectors of Vectors of data.


```racket
#lang typed/racket
;; {{trans|Java}}
(define-type (V2 A) (Vectorof (Vectorof A)))
(define-type VI (Vectorof Integer))
(define-type V2R (V2 Real))
(define-type Q (U 'ε Integer))
(define ε 'ε)
(struct Shipment ([qty : Q] [cost/unit : Real] [r : Integer] [c : Integer]))
(define-type Shipment/? (Option Shipment))
(define-type V2-Shipment/? (V2 Shipment/?))
(define-type Shipment/?s (Listof Shipment/?))
(define-type Shipments (Listof Shipment))

(: Q+ (Q Q -> Q))
(: Q- (Q Q -> Q))
(: Q<? (Q Q -> Boolean))
(: Q-zero? (Q -> Boolean))
(: Q-unary- (Q -> Q))
(: Q*R (Q Real -> Real))

(define Q+ (match-lambda** [('ε 0) ε] [(0 'ε) ε] [('ε 'ε) ε] [('ε x) x] [(x 'ε) x]
                           [((? integer? x) (? integer? y)) (+ x y)]))

(define Q<? (match-lambda** [('ε 0) #f] [(0 'ε) #t] [('ε 'ε) #f] [('ε x) #t] [(x 'ε) #f]
                            [((? integer? x) (? integer? y)) (< x y)]))

(define Q- (match-lambda** [('ε 0) ε] [(0 'ε) ε] [('ε 'ε) 0] [('ε (? integer? x)) (- x)] [(x 'ε) x]
                           [((? integer? x) (? integer? y)) (- x y)]))

(define Q-unary- (match-lambda ['ε ε] [(? integer? x) (- x)]))

(define Q-zero? (match-lambda ['ε #f] [(? integer? x) (zero? x)]))

(define Q*R (match-lambda** [('ε _) 0] [((? integer? x) y) (* x y)]))

(: vector-ref2 (All (A) ((Vectorof (Vectorof A)) Integer Integer -> A)))
(define (vector-ref2 v2 r c) (vector-ref (vector-ref v2 r) c))

(: vector-set!2 (All (A) ((Vectorof (Vectorof A)) Integer Integer A -> Void)))
(define (vector-set!2 v2 r c v) (vector-set! (vector-ref v2 r) c v))

(define (northwest-corner-rule! [supply : VI] [demand : VI] [costs : V2R] [M : V2-Shipment/?]) : Void
  (define supply-l (vector-length supply))
  (define demand-l (vector-length demand))
  (let loop ((r 0) (nw 0) (c 0))
    (cond [(= r supply-l) (void)]
          [(= c demand-l) (loop (add1 r) nw 0)]
          [else
           (define quantity (min (vector-ref supply r) (vector-ref demand c)))
           (cond
             [(positive? quantity)
              (define shpmnt (Shipment quantity (vector-ref2 costs r c) r c))
              (vector-set!2 M r c shpmnt)
              (define supply-- (- (vector-ref supply r) quantity))
              (define demand-- (- (vector-ref demand c) quantity))
              (vector-set! supply r supply--)
              (vector-set! demand c demand--)
              (if (zero? supply--) (loop (add1 r) c 0) (loop r nw (add1 c)))]
             [else (loop r nw (add1 c))])])))

(define (stepping-stone! [supply : VI] [demand : VI] [costs : V2R] [M : V2-Shipment/?]) : Void
  (fix-degenerate-case! supply demand costs M)
  (define-values (move leaving max-reduction)
    (for*/fold : (Values Shipments Shipment/? Real)
      ((move : Shipments null) (leaving : Shipment/? #f) (max-reduction : Real 0))
      ((r (vector-length supply))
       (c (vector-length demand))
       (m (in-value (vector-ref2 M r c)))
       #:unless m)
      (define path (let ((trial (Shipment 0 (vector-ref2 costs r c) r c))) (get-closed-path trial M)))
      (define-values (+? reduction leaving-cand lowest-quantity)
        (for/fold : (Values Boolean Real Shipment/? (Option Q))
          ((+? #t) (reduction : Real 0) (leaving-cand : Shipment/? #f) (lowest-q : (Option Q) #f))
          ((s (in-list path)))
          (define s.cpu (Shipment-cost/unit s))
          (if +?
              (values #f (+ reduction s.cpu) leaving-cand lowest-q)
              (let ((reduction-- (- reduction s.cpu))
                    (s.q (Shipment-qty s)))
                (if (or (not lowest-q) (Q<? s.q lowest-q))
                    (values #t reduction-- s s.q)
                    (values #t reduction-- leaving-cand lowest-q))))))
      
      (if (< reduction max-reduction)
          (values path leaving-cand reduction)
          (values move leaving max-reduction))))
  
  (unless (null? move)
    (define l.q (Shipment-qty (cast leaving Shipment)))
    (for/fold ((+? : Boolean #t)) ((s (in-list move)))
      (define s.q+ ((if +? Q+ Q-) (Shipment-qty s) l.q))
      (define s+ (struct-copy Shipment s [qty s.q+]))
      (vector-set!2 M (Shipment-r s+) (Shipment-c s+) (if (Q-zero? s.q+) #f s+))
      (not +?))
    (stepping-stone! supply demand costs M)))

(: matrix->list (All (T) ((V2 T) -> (Listof T))))
(define (matrix->list m)
  (for*/list : (Listof T) ((r (in-vector m)) (c (in-vector r)) #:when c)
    c))

(define (fix-degenerate-case! [supply : VI] [demand : VI] [costs : V2R] [M : V2-Shipment/?]) : Void
  (define m-list (matrix->list M))
  (unless (= (+ (vector-length supply) (vector-length demand) -1) (length m-list))
    (let/ec ret : Void
      (for* ((r (vector-length supply)) (c (vector-length demand)) #:unless (vector-ref2 M r c))
        (define dummy (Shipment ε (vector-ref2 costs r c) r c))
        (when (null? (get-closed-path dummy M))
          (vector-set!2 M r c dummy)
          (ret (void)))))))

(: get-closed-path (Shipment V2-Shipment/? -> Shipments))
(define (get-closed-path s matrix)
  ; remove (and keep removing) elements that do not have a vertical AND horizontal neighbour
  (define path
    (let loop : Shipment/?s
      ((path (cons s (matrix->list matrix))))
      (define (has-neighbours [e : Shipment/?]) : Boolean
        (match-define (list n0 n1) (get-neighbours e path))
        (and n0 n1 #t))
      (define-values (with-nbrs w/o-nbrs)
        ((inst partition Shipment/? Shipment/?) has-neighbours path))
      (if (null? w/o-nbrs) with-nbrs (loop with-nbrs))))
  
  ;; place the remaining elements in the correct plus-minus order
  (define p-len (length path))
  (define-values (senots prev)
    (for/fold : (Values Shipments Shipment/?)
      ((senots : Shipments null) (prev : Shipment/? s))
      ((i p-len))
      (values (if prev (cons prev senots) senots)
              (list-ref (get-neighbours prev path) (modulo i 2)))))
  (reverse senots))

(define (get-neighbours [s : Shipment/?] [lst : Shipment/?s]) : (List Shipment/? Shipment/?)
  (define-values (n0 n1)
    (for/fold : (Values Shipment/? Shipment/?)
      ((n0 : Shipment/? #f) (n1 : Shipment/? #f))
      ((o (in-list lst)) #:when (and o s) #:unless (equal? o s))              
      (values (or n0 (and (= (Shipment-r s) (Shipment-r o)) o))
              (or n1 (and (= (Shipment-c s) (Shipment-c o)) o)))))
  (list n0 n1))

(define (print-result [S : VI] [D : VI] [M : V2-Shipment/?] [fmt : String] . [args : Any *]) : Real
  (apply printf (string-append fmt "~%") args)
  (define total-costs
    (for*/sum : Real
      ((r (vector-length S)) (c (vector-length D)))
      (when (zero? c) (unless (zero? r) (newline)))
      (define s (vector-ref2 M r c))
      (cond
        [(and s (= (Shipment-r s) r) (= (Shipment-c s) c))
         (define q (Shipment-qty s))
         (printf "\t~a" q)
         (Q*R q (Shipment-cost/unit s))]
        [else (printf "\t-") 0])))
  (printf "~%Total costs: ~a~%~%" total-costs)
  total-costs)

;; inits from current-input-port --- make sure you set that before coming in
(define (init) : (Values VI VI V2R V2-Shipment/?)
  (define n-sources (cast (read) Integer))
  (define n-destinations (cast (read) Integer))
  (define srcs. (for/list : (Listof Integer) ((_ n-sources)) (cast (read) Integer)))
  (define dsts. (for/list : (Listof Integer) ((_ n-destinations)) (cast (read) Integer)))
  
  (define sum-src--sum-dest (- (apply + srcs.) (apply + dsts.)))
  
  (define-values (supply demand)
    (cond [(positive? sum-src--sum-dest) (values srcs. (append dsts. (list sum-src--sum-dest)))]
          [(negative? sum-src--sum-dest) (values (append srcs. (list (- sum-src--sum-dest))) dsts.)]
          [else (values srcs. dsts.)]))
  
  (define s-l (length supply))
  (define d-l (length demand))
  (define costs (for/vector : V2R ((_ s-l)) ((inst make-vector Real) d-l 0)))
  (define matrix (for/vector : V2-Shipment/? ((_ s-l)) ((inst make-vector Shipment/?) d-l #f)))
  (for* ((i n-sources) (j n-destinations)) (vector-set!2 costs i j (cast (read) Real)))
  (values (list->vector supply) (list->vector demand) costs matrix))

(: transportation-problem (Input-Port -> Real))
(define (transportation-problem p)
  (parameterize ([current-input-port p])
    (define name (read))
    (define-values (supply demand costs matrix) (init))
    (northwest-corner-rule! supply demand costs matrix)
    (stepping-stone! supply demand costs matrix)
    (print-result supply demand matrix "Optimal solutions for: ~s" name)))

(module+ test
  (require typed/rackunit)
  (define (check-tp [in-str : String] [expected-cost : Real])
    (define cost ((inst call-with-input-string Real) in-str transportation-problem))
    (check-equal? cost expected-cost))

  (check-tp #<<$
input1
 2 3
25 35
20 30 10
3 5 7
3 2 5
$
            180)
  
  (check-tp #<<$
input2
3 3
12 40 33
20 30 10
3 5 7
2 4 6
9 1 8 
$
            130)

  (check-tp #<<$
input3
4 4
14 10 15 12
10 15 12 15
10 30 25 15
20 15 20 10
10 30 20 20
30 40 35 45
$
            1000))
```

Output of: <code>raco test Transportation-problem.rkt</code>:

```txt
raco test: (submod "transportation-problem.rkt" test)
Optimal solutions for: input1
        20      -       5
        -       30      5
Total costs: 180

Optimal solutions for: input2
        -       -       -       12
        20      -       10      10
        -       30      -       3
Total costs: 130

Optimal solutions for: input3
        -       -       -       14
        -       9       -       1
        10      -       5       -
        -       5       7       -
        -       1       -       -
Total costs: 1000

3 tests passed

```



## SAS

Use network solver in SAS/OR:


```sas
/* create SAS data sets */
data cost_data;
   input from $ to $ cost; 
   datalines;
s1 c1 3
s1 c2 5
s1 c3 7
s2 c1 3
s2 c2 2
s2 c3 5
;

data supply_data;
   input node $ supply;
   datalines;
s1  25
s2  35
c1 -20
c2 -30
c3 -10
;

/* call OPTMODEL procedure in SAS/OR */
proc optmodel;
   /* declare sets and parameters, and read input data */
   set <str,str> LINKS;
   num cost {LINKS};
   read data cost_data into LINKS=[from to] cost;
   set NODES = union {<i,j> in LINKS} {i,j};
   num supply {NODES} init 0;
   read data supply_data into [node] supply;
   num flow {LINKS};

   /* call network solver */
   solve with network /
      mincostflow links=(weight=cost) nodes=(weight=supply) direction=directed out=(flow=flow);

   /* print optimal solution */
   print _OROPTMODEL_NUM_['OBJECTIVE'];
   print flow;
quit;
```


Output:

```txt

180 

flow 
  c1 c2 c3 
s1 20 0 5 
s2 0 30 5 

```

