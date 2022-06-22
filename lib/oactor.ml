(* Module Oactor *)
type 'a t = {
  scheduler : Multiroundrobin.t;
  mutable methods : 'a Option.t;
  mutable domain : Domain.id
}

let send actor process =
  Multiroundrobin.push_process actor.scheduler process

let methods actor = Option.get actor.methods

let in_same_domain actor =
  actor.domain = Domain.self ()

let create methods =
  let scheduler, domain = Multiroundrobin.create () in
  let self = {
    scheduler;
    methods = None;
    domain;
  } in
  self.methods <- Some (methods self);
  self
