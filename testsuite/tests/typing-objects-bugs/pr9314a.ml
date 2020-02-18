(* TEST
   * expect
*)

type 'a alpha = 'b constraint 'a = < alpha : 'b>
type 'a beta = 'b constraint 'a = < beta : 'b>

type 'a gamma = 'b constraint 'a = < gamma : 'b; delta : _>
type 'a delta = 'b constraint 'a = < delta : 'b; gamma : _>

type 'a alpha_of_gamma = 'a gamma alpha
type 'a beta_of_delta = 'a delta beta

type ('a, 'b) alphabeta

module Alphabeta = struct
  type ('contains_beta, 'just_alpha) t = { alphabeta : ('just_alpha, 'contains_beta beta) alphabeta }
end

type 'a t =
  { other : int
  ; alphabeta : ('a alpha_of_gamma, 'a beta_of_delta) alphabeta
  }

let create
      (input : ('a delta, 'a alpha_of_gamma) Alphabeta.t)
  : 'a t
  =
  let t =
    { other = 0 
    ; alphabeta = input.alphabeta
    }
  in
  t
;;
[%%expect{|
type 'a alpha = 'b constraint 'a = < alpha : 'b >
type 'a beta = 'b constraint 'a = < beta : 'b >
type 'a gamma = 'b constraint 'a = < delta : 'c; gamma : 'b >
type 'a delta = 'b constraint 'a = < delta : 'b; gamma : 'c >
type 'a alpha_of_gamma = 'a gamma alpha
  constraint 'a = < delta : 'b; gamma : < alpha : 'c > >
type 'a beta_of_delta = 'a delta beta
  constraint 'a = < delta : < beta : 'b >; gamma : 'c >
type ('a, 'b) alphabeta
module Alphabeta :
  sig
    type ('a, 'just_alpha) t = {
      alphabeta : ('just_alpha, 'a beta) alphabeta;
    } constraint 'a = < beta : 'b >
  end
type 'a t = {
  other : int;
  alphabeta : ('a alpha_of_gamma, 'a beta_of_delta) alphabeta;
} constraint 'a = < delta : < beta : 'b >; gamma : < alpha : 'c > >
val create :
  ((< delta : < beta : 'a beta_of_delta >;
      gamma : < alpha : 'a alpha_of_gamma > >
    as 'a)
   delta, 'a alpha_of_gamma)
  Alphabeta.t -> 'a t = <fun>
|}, Principal{|
type 'a alpha = 'b constraint 'a = < alpha : 'b >
type 'a beta = 'b constraint 'a = < beta : 'b >
type 'a gamma = 'b constraint 'a = < delta : 'c; gamma : 'b >
type 'a delta = 'b constraint 'a = < delta : 'b; gamma : 'c >
type 'a alpha_of_gamma = 'a gamma alpha
  constraint 'a = < delta : 'b; gamma : < alpha : 'c > >
type 'a beta_of_delta = 'a delta beta
  constraint 'a = < delta : < beta : 'b >; gamma : 'c >
type ('a, 'b) alphabeta
module Alphabeta :
  sig
    type ('a, 'just_alpha) t = {
      alphabeta : ('just_alpha, 'a beta) alphabeta;
    } constraint 'a = < beta : 'b >
  end
type 'a t = {
  other : int;
  alphabeta : ('a alpha_of_gamma, 'a beta_of_delta) alphabeta;
} constraint 'a = < delta : < beta : 'b >; gamma : < alpha : 'c > >
val create :
  (< delta : < beta : < delta : < beta : 'a >; gamma : < alpha : 'b > >
                      beta_of_delta >;
     gamma : < alpha : < delta : < beta : 'a >; gamma : < alpha : 'b > >
                       alpha_of_gamma > >
   delta,
   < delta : < beta : < delta : < beta : 'a >; gamma : < alpha : 'b > >
                      beta_of_delta >;
     gamma : < alpha : < delta : < beta : 'a >; gamma : < alpha : 'b > >
                       alpha_of_gamma > >
   alpha_of_gamma)
  Alphabeta.t ->
  < delta : < beta : < delta : < beta : 'a >; gamma : < alpha : 'b > >
                     beta_of_delta >;
    gamma : < alpha : < delta : < beta : 'a >; gamma : < alpha : 'b > >
                      alpha_of_gamma > >
  t = <fun>
|}];;
