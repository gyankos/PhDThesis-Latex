(** Utility function, treating ls as sets. It removes duplicated instances *)
let unique ls = List.sort_uniq compare ls
  
(** Unility function, flattening and treating ls as sets *)
let uFlat ls = unique (List.flatten ls)
  
(**
   Returns an equivalence class between the elements of outer and inner. The most
   common case is having inner and outer as the same set.

   outer = Set over which the class are created
   inner = Set over which the class are created from the elements of inner
   g = function combining the classes in outer and the inner elements matching the class
   f = equivalence between the elements and the class 

   { { g(x,{ y \in inner | f(x,y)}) } | x \in outer }
*)
let toClass outer inner g f =
  unique (List.map (fun x -> g x (List.filter (fun y -> f x y) inner)) outer)
  
(** More handy that a toClass function, where there is one single set over which perform 
    the classification *)
let group_by (ff : 'a -> 'b) (g : 'a -> 'c) (ll : 'a list) :
  ('b, 'c list) Hashtbl.t =
  List.fold_left
    (fun acc e ->
       let grp = ff e in
       let grp_mems = try Hashtbl.find acc grp with | Not_found -> []
       in (Hashtbl.replace acc grp ((g e) :: grp_mems); acc))
    (Hashtbl.create 100) ll
  
(** Return the group_by result within a list *)
let list_group_by f g ll =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) (group_by f g ll) []
  
let pair a b = (a, b)
  
let linject x = [ x ]
  
let limax def = function | [] -> def | x :: xs -> List.fold_left max x xs
  
let lomin = function | [] -> None | x :: xs -> Some (List.fold_left min x xs)
  
let lomax = function | [] -> None | x :: xs -> Some (List.fold_left max x xs)
  
let rec range m mm =
  if m == mm then [ m ] else if m > mm then [] else m :: (range (m + 1) mm)
  
let is_some = function | Some _ -> true | _ -> false
  
let get = function | Some a -> a | _ -> failwith "missing value"
  
module IntSet =
  Set.Make(struct let compare = Pervasives.compare
                     type t = int
                      end)
  
(** Defining a function with finite domain *)
type ('a, 'b) funzione = { dom : 'a list; f : 'a -> 'b }

let emptyFun c = { dom = []; f = (fun x -> c); }
  
let domain f = f.dom
  
let expand f = f.f
  
(** Definition of a gsm as provided in the theorical framework *)
type gsm =
  { o : int; oO : int list; ell : (int, string list) funzione;
    xi : (int, string list) funzione;
    phi : (int, (string, int list) funzione) funzione
  }

let egsm reference =
  {
    o = reference;
    oO = [];
    ell = emptyFun [];
    xi = emptyFun [];
    phi = emptyFun (emptyFun []);
  }
  
(** Practical implementation, more handy to initialize, as provided by the
    Java implementation *)
type gsm_object =
  { id : int; e : string list; x : string list;
    p : (string * (int list)) list
  }

(* f + [i -> l] *)
let appendList i l ff =
  { dom = i :: ff.dom; f = (fun x -> if x = i then l else ff.f x); }
  
(* f + [i -> x -> l | (x,l) \in ls] *)
let cpListToFunction i ls fu =
  if (List.length ls) = 0
  then fu
  else
    {
      dom = i :: fu.dom;
      f =
        (fun j ->
           if j = i
           then
             List.fold_right (fun (s, il) ff -> appendList s il ff) ls
               (fu.f i)
           else fu.f j);
    }
  

(** Appends a gsm_object into the gsm theorical model *)
let appendElement (iexp : gsm_object) (gsm_elem : gsm) : gsm =
  {
    o = gsm_elem.o;
    oO = iexp.id :: gsm_elem.oO;
    ell = appendList iexp.id iexp.e gsm_elem.ell;
    xi = appendList iexp.id iexp.x gsm_elem.xi;
    phi = cpListToFunction iexp.id iexp.p gsm_elem.phi;
  }
  
(** Given the reference object ~reference creates a gsm from the ~gol list *)
let gsm_object_list_to_gsm (reference : int) (gol : gsm_object list) : gsm =
  List.fold_right (fun elem acc -> appendElement elem acc) gol
    (egsm reference)
  
(* \varphi_{gsm} *)
let varphi gsm =
  let www = gsm.phi
  in
    (cpListToFunction ()
       (List.map
          (fun x ->
             (x, (let yyy = www.f x in uFlat (List.map yyy.f yyy.dom))))
          www.dom)
       (emptyFun (emptyFun []))).
      f ()
  
(* \varphi^{step}_{gsm}(o) *)
let rec varphiRec (step : int) gsm o =
  if step = 0
  then [ o ]
  else
    if step = 1
    then (varphi gsm).f o
    else
      unique
        (List.flatten (List.map (varphi gsm).f (varphiRec (step - 1) gsm o)))
  
(** Returning the maximum height of the gsm, at any object *)
let overallGsmHeight gsm =
  let maxHeightO o =
    let rec inner ls acc =
      if (List.length ls) == 0
      then acc
      else inner (uFlat (List.map (varphi gsm).f ls)) (acc + 1) in
    let l = (varphi gsm).f o in if (List.length l) == 0 then 0 else inner l 1
  in limax 0 (List.map (fun o -> maxHeightO o) gsm.oO)
  
let varphiplus gsm o =
  uFlat
    (List.map (fun x -> varphiRec x gsm o) (range 1 (overallGsmHeight gsm)))
  
let varphistar gsm (o : int) =
  unique
    (o ::
      (uFlat
         (List.map (fun x -> varphiRec x gsm o)
            (range 1 (overallGsmHeight gsm)))))
  
(** returns the elements of gsm of which o is a content *)
let containerOf gsm o =
  let w = varphi gsm
  in
    unique
      (List.map fst
         (List.filter (fun (i, l) -> List.mem o l)
            (List.map (fun x -> (x, (w.f x))) w.dom)))
  
(** returns the elements of gsm of which o is a content, alongside with the label associated to it *)
let containerOfWithLabel g o =
  List.flatten
    (List.map
       (fun x ->
          let y = g.phi.f x
          in
            List.map fst
              (List.filter (fun ((x, y), l) -> List.mem o l)
                 (List.map (fun z -> ((x, z), (y.f z))) y.dom)))
       g.phi.dom)
  
(** Returns the basic correspondences to which o is associated to *)
let cvBase g o = (*@\label{v3:cvBase}@*)
  List.append (g.xi.f o) (List.map snd (containerOfWithLabel g o))
  
let cvBase2 g o = g.xi.f o
  
let cvRecAll cvBaseFun (step : int) g o =
  uFlat (List.map (cvBaseFun g) (varphistar g o))
  
let cvRec step g o = cvRecAll cvBase step g o
  
let cvRec2 step g o = cvRecAll cvBase2 step g o
  
(**
 *  After initializing the function with the following elements, it returns if the
 *  two elements match because of a shared common value
 * 
 *  l = value extraction function to be applied on the left
 *  r = value extraction function to be applied on the right
 *  step = depth step on both elements to go in order to search the common values
 *  gl = gsm for the left elements 
 *  gr = gsm for the right elements
 *  vartheta = binary predicate creating the equivalences
*)
let cvTest l r step gl gr vartheta o op = (*@\label{v3:cvTest}@*)
  List.exists
    (fun a -> List.exists (fun ap -> vartheta a ap) (cvRecAll r step gr op))
    (cvRecAll l step gl o)
  
(* Function to be used when the data to be compared belong to the same type, and hence the parts where to extract the data are similar *)
let cvTestData vef step g vartheta = cvTest vef vef step g g vartheta
  
type correspondence = { src : int; dst : int }

let oe x y = { dst = x; src = y; }
  
type morphism = { schema : int; data : int list }

let mo x y = { schema = x; data = y; }
  
(** These are all the kinds of schema alignments that we can achieve on one single
     correspondence *)
type schema_alignments =
  { ell_corr : correspondence list; xi_onelements : correspondence list;
    xi_contents : correspondence list
  }

let sa x y z = { ell_corr = x; xi_onelements = y; xi_contents = z; }
  
(** These are all the possible data inputs over which we play on *)
type data_input =
  { hub_schema_gsm : gsm; source_schema_gsm : gsm; data_gsm : gsm
  }

let edata x y z =
  { hub_schema_gsm = egsm x; source_schema_gsm = egsm y; data_gsm = egsm z; }
  
type ell_corr = correspondence list

type xi_onelements = correspondence list

type xi_contents = correspondence list

type morphisms = morphism list

(** Filters the ell_corr having o as a target *)
let getEllO (ec : ell_corr) omega = List.filter (fun x -> x.dst = omega) ec
  
(** Filters the ell_onelements having o as a target *)
let getExprO (ec : xi_onelements) omega =
  List.filter (fun x -> x.dst = omega) ec
  
(** Returns the set of objects matched to the schema through schemaId *)
let getW (ml : morphisms) schemaId =
  uFlat
    (List.map (fun x -> x.data)
       (List.filter (fun x -> x.schema = schemaId) ml))
  
(* Returning the data elements matching with the ell definition *)
(** Returns the \mathcal{I} associated to the currently evaluated object *)
let calI (ec : schema_alignments) (ml : morphisms) omega =
  uFlat (List.map (fun x -> getW ml x.src) (getEllO ec.ell_corr omega))
  
(* Filtering the ell by the matchings with the xi values *)
(** Returns the set of objects matched to the schema through schemaId, where
    the source is kept distinct per morphism *)
let getWSplitted (ml : morphisms) schemaId =
  List.map (fun x -> x.data) (List.filter (fun x -> x.schema = schemaId) ml)
  
let eee (ec : xi_onelements) (ml : morphisms) omega =
  uFlat (List.map (fun x -> getWSplitted ml x.src) (getExprO ec omega))
  
(* Given a collection of n elements of ('a list list), it joins the elements together *)
let rec join ls =
  let rec joining ls acc =
    match ls with
    | a :: b ->
        if (List.length a) = 0
        then joining b acc
        else
          joining b
            (List.flatten
               (List.map (fun x -> List.map (fun y -> x @ y) a) acc))
    | [] -> acc
  in
    match ls with
    | [] -> []
    | [ a ] -> a
    | a :: b -> if (List.length a) = 0 then join b else joining b a
  
(* Given a collection of n elements of ('a list list), it creates the cross product *)
let rec cross ls =
  let rec crossing ls acc =
    match ls with
    | a :: b ->
        if (List.length a) = 0
        then crossing b acc
        else
          crossing b
            (List.flatten
               (List.map (fun x -> List.map (fun y -> x @ (linject y)) a) acc))
    | [] -> acc
  in
    match ls with
    | [] -> []
    | [ a ] -> if (List.length a) = 0 then [] else linject a
    | a :: b ->
        if (List.length a) = 0
        then cross b
        else crossing b (List.map linject a)
  
(* Same function as cross, but keeps the schema information as the left part 
   when the lists preserve the information from where they came from *)
let rec pair_cross ls = ((List.map fst ls), (cross (List.map snd ls)))
  
(* Same function as cross, but it is used on the incoming values from the contents *)
let rec pair_cross2 ls =
  ((List.flatten (List.map fst ls)), (uFlat (cross (List.map snd ls))))
  
(**
 * 
 * extractor = function used to select the right alignment from "sa"
 * sa        = alignments to be selected via extractor
 * ml        = morphisms going from the source schema to the data
 * omega     = element originating from the hub schema
 *)
let expandAnyMorphismOverObject2 extractor (sa : schema_alignments)
                                 (ml : morphisms) (omega : int) =
  (* I want to select all the xi mappings (over object) that have omega as a target 
      srcw = { x.src | x\in (sa.extractor), x.dst = omega }
   *)
  let srcw =
    List.map (fun x -> x.src)
      (List.filter (fun x -> x.dst = omega) (extractor sa)) in
  (* I want to select all the morphisms that have their schema eleme tin srcw 
      mlFilt = { x\in ml | x.schema \in srcw }
             = { y\in ml | x\in (sa.extractor), x.dst = omega, x.src = y.schema }
   *)
  let mlFilt = List.filter (fun x -> List.mem x.schema srcw) ml
  in
    (* performs a group by over the mlFilt by source schema elements*)
    pair_cross (list_group_by (fun x -> x.schema) (fun x -> x.data) mlFilt)
  

let expandXiMorphismsOverObject (sa : schema_alignments) (ml : morphisms)
                                (omega : int) =
  expandAnyMorphismOverObject2 (fun x -> x.xi_onelements) sa ml omega
  
let expandEllMorphismsOverObject (sa : schema_alignments) (ml : morphisms)
                                 (omega : int) =
  expandAnyMorphismOverObject2 (fun x -> x.ell_corr) sa ml omega
  
let rec listifte f =
  function | [] -> [] | a :: b -> (if f a then a else []) :: (listifte f b)
  
(** This function filters the ells that match with the correspondent \xi values
*)
let crossLists vef step vartheta (datei : data_input) (schema, ells) (*@\label{v3:crossLists}@*)
               (schema2, xixs) =
  if (List.length ells) = 0
  then (schema2, xixs)
  else
    if (List.length xixs) = 0
    then (schema, ells)
    else
      (schema,
       (List.map
          (fun bigL ->
             listifte
               (fun l ->
                  List.exists
                    (fun bigE ->
                       List.exists
                         (fun e ->
                            List.for_all
                              (fun vare ->
                                 List.exists
                                   (fun lam ->
                                      cvTestData vef step datei.data_gsm
                                        vartheta vare lam)
                                   l)
                              e)
                         bigE)
                    xixs)
               bigL)
          ells))
  
let crossMorphisms vef step vartheta (datei : data_input) (*@\label{v3:crossMorphisms}@*)
                   (sa : schema_alignments) (ml : morphisms) (omega : int) =
  let (schema, ells) = expandEllMorphismsOverObject sa ml omega in
  let (schema2, xixs) = expandXiMorphismsOverObject sa ml omega
  in
    (* If I only have xi-matches and no \lambda ones, I return directly the data referenced by the source*)
    crossLists vef step vartheta datei (schema, ells) (schema2, xixs)
  
(** Definition of the relatve height *)
let rh (g : gsm) o op = (*@\label{v3:rh}@*)
  let h = overallGsmHeight g in
  let r = range 1 h
  in
    match lomax
            (List.map (fun c -> c + 1)
               (List.filter (fun n -> List.mem op (varphiRec n g o)) r))
    with
    | Some n -> Some n
    | None ->
        (match lomax
                 (List.map (fun c -> c + 1)
                    (List.filter (fun n -> List.mem o (varphiRec n g op)) r))
         with
         | Some n -> Some (- n)
         | None ->
             if
               (o = op) ||
                 (List.exists
                    (fun opp ->
                       (List.mem o ((varphi g).f opp)) &&
                         (List.mem op ((varphi g).f opp)))
                    (List.filter (fun x -> (( != ) x o) || (( != ) x op))
                       g.oO))
             then Some 0
             else None)
  
let h g = (*@\label{v3:h}@*)
  limax 0
    (List.map
       (fun op ->
          match rh g g.o op with
          | Some n -> if n >= 0 then n else 0
          | None -> 0)
       (varphistar g g.o))
  
let ho gsm o curr = (*@\label{v3:ho}@*)
  let ll =
    List.map (fun x -> rh gsm curr x)
      (List.filter (fun x -> ( != ) x curr) (varphistar gsm o))
  in limax 0 (List.map get (List.filter is_some ll))
  
let contentSort gsm o = (*@\label{v3:contentSort}@*)
  List.sort (fun x y -> compare (- (ho gsm o x)) (- (ho gsm o y)))
    ((varphi gsm).f o)
  
(** Returns the indices indicating which elements has to be extracted to perform
 *  the object selection.
 *
 *  xischemas = elements over which extract the filtering
 *  ls = list providing the relevant elements to be extractes
 *  headers = elements originating from the lambda-matches from the hub schema,
 *            from which the hub schema elements are referenced
 *)
let filterIndicesFromContents xischemas ls headers =
  List.map fst
    (List.filter
       (fun (i, xsl) ->
          List.exists (fun x -> (x.src = xsl) && (List.mem xsl headers))
            xischemas)
       (List.mapi pair ls))
  
let rec (*@\label{v3:postVisitPace}@*)
  postVisitPace visit vef step vartheta (datei : data_input)
                (sa : schema_alignments) (ml : morphisms) (omega : int) =
  (* Performs a postVisit on the contents. I choose the contents by their relative
   * height with respect to the containment relations in o
   *)
  let (schema, results) = (*@\label{v3:postVisit}@*)
    pair_cross2
      (List.map (visit datei) (contentSort datei.hub_schema_gsm omega)) in
  (* Return all the lambda-matches associated to omega *)
  let currentElls = getEllO sa.ell_corr omega in
  (* contents' filtered schemas referencing only to the elements that truly have to be matched *)
  let indices =
    filterIndicesFromContents sa.xi_contents
      (varphistar datei.source_schema_gsm omega) schema in
  (* Filtering the second-xi-matches having as sources the source schema objects appearing within the results*)
  let (fs, fr) =
    ((List.map (List.nth schema) indices),
     (List.map (fun ls -> List.map (List.nth ls) indices) results))
  in
    crossLists vef step vartheta datei
      (crossMorphisms vef step vartheta datei sa ml omega) (fs, fr)
  
let performAssociations vef step vartheta datei sa ml = (*@\label{v3:performAssociations}@*)
  (** Memoizing the visit steps. This is possible due to postVisit *)
  let cache = Hashtbl.create (List.length datei.hub_schema_gsm.oO) in
  let rec postVisit omega =
    try Hashtbl.find cache omega
    with
    | Not_found ->
        (* Performs a postVisit on the contents. I choose the contents by their 
         * relative height with respect to the containment relations in o.
         * schema = $\mathcal{S}_{o_i}$
         * results = $\mathcal{I}_{o_i}$
         *)
        let (schema, results) =
          pair_cross2
            (List.map postVisit (contentSort datei.hub_schema_gsm omega)) in
        (* Return all the lambda-matches associated to omega *)
        let currentElls = getEllO sa.ell_corr omega in
        (* contents' filtered schemas referencing only to the elements that 
         * truly have to be matched *)
        let indices =
          filterIndicesFromContents sa.xi_contents
            (varphistar datei.source_schema_gsm omega) schema in
        (* Filtering the second-xi-matches having as sources the source schema 
         * objects appearing within the results*)
        let (fs, fr) =
          ((List.map (List.nth schema) indices),
           (List.map (fun ls -> List.map (List.nth ls) indices) results)) in
        let f =
          crossLists vef step vartheta datei
            (crossMorphisms vef step vartheta datei sa ml omega) (fs, fr)
        in (Hashtbl.add cache omega f; f)
  in (postVisit datei.hub_schema_gsm.o; cache)
  

(* EXAMPLES *)
let ggg =
  gsm_object_list_to_gsm 0
    [ {
        id = 0;
        e = [ "ciao" ];
        x = [ "expr" ];
        p = [ ("left", [ 2 ]); ("right", [ 3; 5 ]) ];
      }; { id = 3; e = []; x = [ "expr" ]; p = [ ("left", [ 2 ]) ]; };
      { id = 2; e = [ "ciao" ]; x = []; p = [ ("right", [ 4 ]) ]; };
      { id = 5; e = [ "ciao" ]; x = [ "expr" ]; p = []; };
      { id = 4; e = [ "ciao" ]; x = [ "expr" ]; p = []; } ]
  
let xmlist = [ oe 0 100; oe 0 200; oe 0 300 ]
  
let lmlist = [ oe 0 400; oe 0 500 ]
  
let mlist =
  [ mo 100 [ 1; 2; 3; 4 ]; mo 100 [ 2; 3; 4; 5 ]; mo 200 [ 5; 6; 7 ];
    mo 200 [ 8; 9 ]; mo 300 [ 10; 11 ]; mo 400 [ 1; 2; 3; 303; 4 ];
    mo 400 [ 100; 279; 305; 303 ]; mo 400 [ 10 ]; mo 500 [ 8; 9 ] ]
  
let em = expandXiMorphismsOverObject (sa lmlist xmlist []) mlist 0
  
let lm = expandEllMorphismsOverObject (sa lmlist xmlist []) mlist 0
  
let cm =
  crossMorphisms (fun x y -> [ y ]) 0 (fun x y -> x = y) (edata 0 0 0)
    (sa lmlist xmlist []) mlist 0
  

