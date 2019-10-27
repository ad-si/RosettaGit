+++
title = "Bucketsort"
description = ""
date = 2018-03-17T14:04:02Z
aliases = []
[extra]
id = 21710
[taxonomies]
categories = []
tags = []
+++


## QB64


```B64

'* Complexity Class: O(N^2)
TYPE MINMaxRec
    min AS LONG
    max AS LONG
END TYPE

REDIM a(0 TO 1048575) AS DOUBLE
FOR FillArray& = LBOUND(a) TO UBOUND(a)
    a(FillArray&) = RND
NEXT
DoRecurse% = -1
DemoOrder& = 1 '* -1 = descending
BucketSort a(), LBOUND(a), UBOUND(a), DemoOrder&, DoRecurse% '* without the recursive initial call, executiom time is FAR slower.

SUB BucketSort (Array() AS DOUBLE, start AS LONG, finish AS LONG, order&, recurse%)
DIM BS_Local_NBuckets AS INTEGER
DIM BS_Local_ArrayRange AS DOUBLE
DIM BS_Local_N AS LONG
DIM BS_Local_S AS LONG
DIM BS_Local_Z AS LONG
DIM BS_Local_Remainder AS INTEGER
DIM BS_Local_Index AS INTEGER
DIM BS_Local_Last_Insert_Index AS LONG
DIM BS_Local_Current_Insert_Index AS LONG
DIM BS_Local_BucketIndex AS INTEGER
REDIM BSMMrec AS MINMaxRec
GetMinMaxArray Array(), start, finish, BSMMrec
BS_Local_ArrayRange = Array(BSMMrec.max) - Array(BSMMrec.min)
IF BS_Local_ArrayRange > 0 THEN
    BS_Local_NBuckets = 2 * INT(LOG(finish - start + 1) / LOG(2)) + 1
    BS_Local_N = (finish - start)
    BS_Local_Remainder = BS_Local_N MOD BS_Local_NBuckets
    BS_Local_NBuckets = BS_Local_NBuckets - 1
    REDIM BS_Buckets_Array(BS_Local_NBuckets, 0 TO (BS_Local_NBuckets * (1 + (BS_Local_N - BS_Local_Remainder) / BS_Local_NBuckets))) AS DOUBLE
    REDIM BS_Count_Array(0 TO BS_Local_NBuckets) AS LONG
    FOR BS_Local_S = start TO finish
        BS_Local_BucketIndex = BS_Local_NBuckets * ((Array(BS_Local_S) - Array(BSMMrec.min)) / BS_Local_ArrayRange)
        BS_Buckets_Array(BS_Local_BucketIndex, BS_Count_Array(BS_Local_BucketIndex)) = Array(BS_Local_S)
        BS_Count_Array(BS_Local_BucketIndex) = BS_Count_Array(BS_Local_BucketIndex) + 1
    NEXT
    BS_Local_Last_Insert_Index = start
    BS_Local_Current_Insert_Index = start
    FOR BS_Local_S = 0 TO BS_Local_NBuckets
        IF BS_Count_Array(BS_Local_S) > 0 THEN
            BS_Local_Last_Insert_Index = BS_Local_Current_Insert_Index
            FOR BS_Local_Z = 0 TO BS_Count_Array(BS_Local_S) - 1
                Array(BS_Local_Current_Insert_Index) = BS_Buckets_Array(BS_Local_S, BS_Local_Z)
                BS_Local_Current_Insert_Index = BS_Local_Current_Insert_Index + 1
            NEXT
            IF recurse% THEN
                '* Without this, Bucketort() will be much slower
                BucketSort Array(), BS_Local_Last_Insert_Index, BS_Local_Current_Insert_Index - 1, order&, 0
            ELSE
                '* using MergeSort will speed this significantly, however, this will be left as an exercise
                '* MergeSort will keep this sorting algorithm quite competitive.
                InsertionSort Array(), BS_Local_Last_Insert_Index, BS_Local_Current_Insert_Index - 1, order&
            END IF
        END IF
    NEXT
    ERASE BS_Buckets_Array, BS_Count_Array
END IF
END SUB

SUB GetMinMaxArray (array() AS DOUBLE, Start&, finish&, GetMinMaxArray_minmax AS MINMaxRec)
n& = finish& - Start&
t% = n& - 10000 * (n& \ 10000)
IF (t% MOD 2) THEN
    GetMinMaxArray_minmax.min = Start&
    GetMinMaxArray_minmax.max = Start&
    GetGetMinMaxArray_minmaxArray_i = Start& + 1
ELSE
    IF array(Start&) > array(finish&) THEN
        GetMinMaxArray_minmax.max = Start&
        GetMinMaxArray_minmax.min = finish&
    ELSE
        GetMinMaxArray_minmax.min = finish&
        GetMinMaxArray_minmax.max = Start&
    END IF
    GetGetMinMaxArray_minmaxArray_i = Start& + 2
END IF

WHILE GetGetMinMaxArray_minmaxArray_i < finish&
    IF array(GetGetMinMaxArray_minmaxArray_i) > array(GetGetMinMaxArray_minmaxArray_i + 1) THEN
        IF array(GetGetMinMaxArray_minmaxArray_i) > array(GetMinMaxArray_minmax.max) THEN
            GetMinMaxArray_minmax.max = GetGetMinMaxArray_minmaxArray_i
        END IF
        IF array(GetGetMinMaxArray_minmaxArray_i + 1) < array(GetMinMaxArray_minmax.min) THEN
            GetMinMaxArray_minmax.min = GetGetMinMaxArray_minmaxArray_i + 1
        END IF
    ELSE
        IF array(GetGetMinMaxArray_minmaxArray_i + 1) > array(GetMinMaxArray_minmax.max) THEN
            GetMinMaxArray_minmax.max = GetGetMinMaxArray_minmaxArray_i + 1
        END IF
        IF array(GetGetMinMaxArray_minmaxArray_i) < array(GetMinMaxArray_minmax.min) THEN
            GetMinMaxArray_minmax.min = GetGetMinMaxArray_minmaxArray_i
        END IF
    END IF
    GetGetMinMaxArray_minmaxArray_i = GetGetMinMaxArray_minmaxArray_i + 2
WEND
END SUB

SUB InsertionSort (array() AS DOUBLE, start AS LONG, finish AS LONG, order&)
DIM InSort_L_ArrayTemp AS DOUBLE
DIM InSort_L_i AS LONG
DIM InSort_L_j AS LONG
SELECT CASE order&
    CASE 1
        FOR InSort_L_i = start + 1 TO finish
            InSort_L_ArrayTemp = array(InSort_L_i)
            InSort_L_j = InSort_L_i - 1
            DO UNTIL InSort_L_j < start
                IF (InSort_L_ArrayTemp < array(InSort_L_j)) THEN
                    array(InSort_L_j + 1) = array(InSort_L_j)
                    InSort_L_j = InSort_L_j - 1
                ELSE
                    EXIT DO
                END IF
            LOOP
            array(InSort_L_j + 1) = InSort_L_ArrayTemp
        NEXT
    CASE ELSE
        FOR InSort_L_i = start + 1 TO finish
            InSort_L_ArrayTemp = array(InSort_L_i)
            InSort_L_j = InSort_L_i - 1
            DO UNTIL InSort_L_j < start
                IF (InSort_L_ArrayTemp > array(InSort_L_j)) THEN
                    array(InSort_L_j + 1) = array(InSort_L_j)
                    InSort_L_j = InSort_L_j - 1
                ELSE
                    EXIT DO
                END IF
            LOOP
            array(InSort_L_j + 1) = InSort_L_ArrayTemp
        NEXT
END SELECT
END SUB

```

