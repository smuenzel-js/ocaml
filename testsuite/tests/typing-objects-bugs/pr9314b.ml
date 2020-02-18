(* TEST
   * expect
*)

module type T = sig
 type 'a alpha = 'b constraint 'a = < alpha : 'b >
  type 'a beta = 'b constraint 'a = < beta : 'b >
  type 'a gamma = 'b constraint 'a = < delta : 'c; gamma : 'b >
  type 'a delta = 'b constraint 'a = < delta : 'b; gamma : 'c >
  type 'a alpha_of_gamma = 'a gamma alpha
  type 'a beta_of_delta = 'a delta beta
  type ('a, 'b) w = W
  type ('a, 'just_alpha) x = { field : ('a beta, 'just_alpha) w; }
  type 'a t = A of ('a alpha_of_gamma, 'a beta_of_delta) w

  val create :
  ((< delta : < beta : 'a alpha_of_gamma >;
      gamma : < alpha : 'a beta_of_delta > >
    as 'a)
   delta, 'a alpha_of_gamma)
  x -> 'a t
end
[%%expect{|
Uncaught exception: Stack overflow

|}];;
