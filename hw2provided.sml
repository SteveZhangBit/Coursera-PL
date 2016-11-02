(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (x, xs) =
    case xs of
          [] => NONE
        | x'::xs' =>
            if same_string (x, x')
                then SOME xs'
                else case all_except_option (x, xs') of
                      NONE => NONE
                    | SOME ys => SOME (x'::ys)

fun get_substitutions1 (xss, x) =
    case xss of
          [] => []
        | xs::xss' =>
            case all_except_option (x, xs) of
                  NONE => get_substitutions1 (xss', x)
                | SOME ys => ys @ get_substitutions1 (xss', x)

fun get_substitutions2 (xss, x) =
    let fun loop (xss, x, acc) =
            case xss of
                  [] => acc
                | xs::xss' =>
                    case all_except_option (x, xs) of
                          NONE => loop (xss', x, acc)
                        | SOME ys => loop (xss', x, acc @ ys)
    in loop (xss, x, []) end

fun similar_names (xss, name) =
    case name of
        {first=x, middle=y, last=z} =>
            let fun loop subs =
                    case subs of
                          [] => []
                        | sub::subs' => {first=sub, middle=y, last=z}::loop subs'
            in name::loop (get_substitutions2 (xss, x)) end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (s, r) =
    case s of
          Spades => Black
        | Clubs => Black
        | Diamonds => Red
        | Hearts => Red

fun card_value (s, r) =
    case r of
          Jack => 10
        | Queen => 10
        | King => 10
        | Ace => 11
        | Num x => x

fun remove_card (cs, c, e) =
    case cs of
          [] => raise e
        | c'::cs' => if c = c' then cs' else c'::remove_card (cs', c, e)

fun all_same_color cs =
    case cs of
          [] => true
        | c::[] => true
        | c1::c2::cs' => if card_color c1 = card_color c2
                         then all_same_color (c2::cs')
                         else false

fun sum_cards cs =
    let fun loop (cs, acc) =
        case cs of
              [] => acc
            | c::cs' => loop (cs', card_value c + acc)
    in loop (cs, 0) end

fun score (cs, goal) =
    let val sum = sum_cards cs
        val pre_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in if all_same_color cs then pre_score div 2 else pre_score end

fun officiate (cs, mvs, goal) =
    let fun loop (cs, mvs, hcs) =
            case (cs, mvs, hcs) of
                  (_, [], hcs) => score (hcs, goal)
                | ([], Draw::_, hcs) => score (hcs, goal)
                | (c::cs', Draw::mvs', hcs) =>
                    let val sum = score ((c::hcs), goal)
                    in
                        if sum > goal then sum else loop (cs', mvs', c::hcs)
                    end
                | (cs, (Discard c)::mvs', hcs) =>
                    loop (cs, mvs', remove_card (hcs, c, IllegalMove))
    in loop (cs, mvs, []) end


(*Challenge Problem a*)
fun small_card_value (s, r) =
    case r of
          Jack => 10
        | Queen => 10
        | King => 10
        | Ace => 1
        | Num x => x

fun small_sum_cards cs =
    let fun loop (cs, acc) =
        case cs of
              [] => acc
            | c::cs' => loop (cs', small_card_value c + acc)
    in loop (cs, 0) end

fun score_challenge (cs, goal) =
    let val big_sum = sum_cards cs

        fun pre_score sum = if sum > goal then 3 * (sum - goal) else (goal - sum)
        fun score pre_score = if all_same_color cs then pre_score div 2 else pre_score

        fun loop small_sum =
            if small_sum = big_sum
            then pre_score big_sum
            else Int.min (pre_score small_sum, loop (small_sum + 10))

    in score (loop (small_sum_cards cs)) end

fun officiate_challenge (cs, mvs, goal) =
    let fun loop (cs, mvs, goal, hcs) =
            case (cs, mvs, goal, hcs) of
                  (_, [], goal, hcs) => score_challenge (hcs, goal)
                | ([], Draw::_, goal, hcs) => score_challenge (hcs, goal)
                | (c::cs', Draw::mvs', goal, hcs) =>
                    let val sum = small_sum_cards (c::hcs)
                    in
                        if sum > goal
                        then score_challenge (c::hcs, goal)
                        else loop (cs', mvs', goal, c::hcs)
                    end
                | (cs, (Discard c)::mvs', goal, hcs) =>
                    loop (cs, mvs', goal, remove_card (hcs, c, IllegalMove))
    in loop (cs, mvs, goal, []) end

(*fun careful_player (cs, goal) =
    case (cs, goal) of
          (_, 0) => []
        | ([], goal) => if goal > 10
                     then [Draw]
                     else []
        | (c::cs', goal) => if goal > 10
                            then Draw::careful_player (cs', goal - card_value c)
                            else if*)