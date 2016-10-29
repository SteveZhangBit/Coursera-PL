fun is_older (x : int*int*int, y : int*int*int) =
    if #1 x <> #1 y
    then #1 x < #1 y
    else
        if #2 x <> #2 y
        then #2 x < #2 y
        else #3 x < #3 y

fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month (tl dates, month)
    else number_in_month (tl dates, month)

fun number_in_months (dates: (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then hd dates :: dates_in_month (tl dates, month)
    else dates_in_month (tl dates, month)

fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

fun get_nth (xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth (tl xs, n - 1)

fun date_to_string (date : int*int*int) =
    let
        val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
        val year = #1 date
        val month = #2 date
        val day = #3 date
    in
        get_nth (month_names, month) ^ " " ^ (Int.toString day) ^ ", " ^ (Int.toString year)
    end

fun number_before_reaching_sum (sum : int, xs : int list) =
    if sum > hd xs
    then 1 + number_before_reaching_sum(sum - hd xs, tl xs)
    else 0

fun what_month (day : int) =
    let
        val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum (day, days_in_month) + 1
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month (day1) :: month_range (day1 + 1, day2)

fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else
        let
            val sub_old = oldest (tl dates)
        in
            if isSome sub_old andalso is_older (valOf sub_old, hd dates)
            then sub_old
            else SOME (hd dates)
        end

fun remove_duplicate (xs : int list) =
    let
        fun is_duplicate (x : int, xs : int list) =
            if null xs
            then false
            else x = hd xs orelse is_duplicate (x, tl xs)
    in
        if null xs
        then []
        else
            if is_duplicate (hd xs, tl xs)
            then remove_duplicate (tl xs)
            else hd xs :: remove_duplicate (tl xs)
    end


fun number_in_months_challenge (dates: (int*int*int) list, months : int list) =
    number_in_months (dates, remove_duplicate months)
