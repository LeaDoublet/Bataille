#use"types_fonctions.ml";;

(*Doublet Lea groupe 8*)



#use"AP1util.ml";;
(* Ceci est seulement une aide qui me sert a mieux visualiser les types.
type t_card_color = HEART | DIAMOND | CLUB | SPADE ;;

type t_card_rank = int ;;

type t_card = {color : t_card_color ; rank : t_card_rank} ;;

type t_player = {name : string ; hand : t_card list} ;;
=> dans "types_fonctions.ml" *)

(*exercice 1*)
(*j'affecte les boites de t_player a mes parametres de fonction*)
let m_player(nm, hand : string * t_card list) : t_player =
  {name = nm ; hand = hand}
  ;;

  (*exercice 2*)
   (*on veut renvoyer un type t_player * t_player qui est un couple de joueurs le resultat doit donc etre sous la forme(joueur1(ses parametres),joueur2(ses parametres)*)

 let init_play(name1, name2 : string * string) : t_player * t_player =
  ( m_player(name1, first_card_list), m_player(name2,second_card_list))
  ;;

  (*exercice 3*)
  (*je prend pour la variable c correspondant � chaque element de type t_card_color qui correspond � une couleur de carte le mot utilils� en francais lorsque l'on joue. Si le joueur invente une couleur qui n'existe pas la fonction lui indiquera une erreur.*)
  
  (*Ex3:1)*)
  let string_of_card_color(c : t_card_color) : string =
    if c = HEART
    then "coeur"
    else
      if c = DIAMOND
      then "carreau"
      else
        if c = CLUB
        then "trefle"
        else
          if c = SPADE
          then"pique"
          else failwith "couleur de carte incorecte"
  ;;
  
  (*Ex3:2)*)
   (* Pour que la fonction affiche la couleur  peu importe la langue, je vais chercher la boite couleur dans la variable c2 et comme je ne possede pas de moyen pour afficher un type t_card � l'�cran j'utilise la fonction string_of_card_color present dans type_fonctions  *)       

let print_card_info (c2 : t_card) : unit = 
  print_string(string_of_card_rank(c2.rank));
  print_newline();
  print_string(string_of_card_color(c2.color))
;;
  

  (*Ex3:3)*)
(* je reutilise la fonction print_card_info que je viens de creer pour afficher la carte � l'�cran car je ne sais pas le faire autrement. De facon � afficher toutes les cartes je prend ma liste j'affiche la premiere carte, je supprime la premiere carte et je continue jusqu'� ce que le paquet ou ici la liste soit vide, j'ai d�cid� d'utiliser une fonction recurcive et non une boucle while car cela me paraissait plus simple que de cr�er une variable mutable*)

let rec print_card_list(c1 : t_card list) : unit =
  if c1 = []
  then ()
  else print_card_info (fst(c1));
      print_card_list(rem_fst(c1));
                    ;;

  (*Ex3:4)*)
 (* Les informations relatives � un joueur � afficher sont son nom et son paquet de carte. De la meme facon que pr�c�demment, je r�utilise ma pr�c�dente fonction pour afficher la liste de carte du joueur qui fait partie de ses informations, p etant de type t_player je vais chercher sa boite nom et je l'affiche *)
  
  let print_player (p : t_player) : unit =
    print_string(p.name);
    print_newline();
    print_card_list(p.hand);
  ;;
  

    
  (*Ex3:5)*)
  (*De facon �  afficher les deux joueurs je reutilise donc print_player que j'affecte � mes deux variables en parametre correspondant � mes deux joueurs  p1 et p2 . Le nom des variables  etant deja defini par le sujet du projet*)
  
    let print_two_players(p1,p2 : t_player * t_player) : unit =
      print_player(p1);
      print_player(p2)
      ;;
      
        
  (*Exercice 4*)
      (* Les valeurs de la bataille sont: 13 pour l'as; mais pour les cartes allant de 2 � 10 et valet, dame, roi que l'on appelle respectivement 11, 12 et 13, leur valeurs sont plus petites que l'as mais elles suivent l'ordre de leur rang quand � la croissance de leur valeurs. j'ai donc d�cid� d'affecter � la plus petite carte du jeu c'est � dire 2 la valeur 1 et d'augmenter de 1 la valeur � chaque fois que j'augmente( carte 3 valeur = 2, carte 4 valeur = 3). Je me suis donc rendue compte que mis � part l'as qui prend pour valeur 13, les autres cartes prennent pour valeur leur num�ro de rang - 1. *)
      
      let rec battle_value(c3 : t_card) : int =
      
                                    if c3.rank = 1 
                                    then  13
                                    else c3.rank - 1
      ;;

      
      (*premiere version jeu*)                              
        
  (*exercice 5*)
      (*Tout d'abord, je veux savoir si la carte que l'un des joueurs presente est plus forte, moins forte ou �gale � la carte de l'autre joueur. Pour cela je me sers donc de la fonction battle_value cr��e precedemment. Une fois cette �tape pass�e, si le joueur un gagne par exemple, je veux prendre sa premiere carte et l'ajouter � la fin de son paquet et donc supprimer cette premier carte du haut du paquet egalement pour pas qu'elle apparaissent deux fois. Cependant le probleme est que je devais ajouter � la fin du paquet (ou de la liste) du joueur un, la premiere carte du joueur deux pour qu'il l'a "gagne". Pour se faire j'ai �t� oblig� de cr�er des variables contenant la premiere carte de chaque paquet des joueurs que j'ai d�cid� d'appeler c1 et c2. De cette mani�re je peux ajouter la premiere carte du paquet du joueur 2 qui est stock�e dans la variable c2 � la fin du paquet du joueur 1. Puis il me suffit de supprimer la carte du haut du paquet du joueur 2. Etant donn� que play_1_round est un tour de jeu ou les joueurs disposent d'au moins 1 carte chacun je n'ai pas le souci de regarder si les paquets sont vides ou non. La reflexion est la meme pour le cas ou le joueur 2 gagne la carte. Lorsqu'il y � �galit� il me suffit de supprimer la premiere carte de chaque paquet et d'ajouter les premieres cartes gard�es en m�moire dans les variables c1 et c2, � la fin de leurs paquets respectifs. *)
      
  let play_1_round(p1, p2 : t_player * t_player) : t_player * t_player =
    let c1 : t_card = fst(p1.hand) and c2 : t_card = fst(p2.hand) in 
  if battle_value(c1) > battle_value(c2)
  then ((m_player(p1.name, add_lst(add_fst(rem_fst(p1.hand),c1),c2))), m_player(p2.name,(rem_fst(p2.hand))))
  
  
  else
    if battle_value(c1) < battle_value(c2)
    then( m_player(p1.name,(rem_fst(p1.hand)))),(m_player(p2.name, add_lst(add_fst(rem_fst(p2.hand),c2),c1))
    )
    else
    ( (m_player(p1.name,add_lst(rem_fst(p1.hand), c1)))),
        (m_player(p2.name,add_lst(rem_fst(p2.hand), c2)))
      ;;
      
      (*exercice 6*)
       (*Pour cette fonction ci cette fois, il y avait la possibilit� qu'un des deux joueurs ne possede plus de carte, ce cas incluant que le joueur adverse serait directement declare  vainqueur. Je verrifie donc dans un premier temps si les joueurs possedent des cartes ou non dans leurs paquets respectifs. Si ce n'est pas le cas je renvoie directement un string annoncant la victoire du joueur adverse. Pour le dernier cas ou les deux joueurs possedent des cartes, mon probleme est que lorsque je joue plus d'un tour il faut que les paquets suivent la partie, j'ai donc cree la maniere de l'exercice precedent deux nouvelles variables (np1 et np2 pour new) qui correspondent aux paquets obtenus apres le tour de jeu. J'ai,  de facon � ce que les tours de jeux se suivent jusqu'ici la victoire d'un des joueurs cree une fonction play_rounds recursive. *)
      
       let rec play_rounds(p1, p2 : t_player * t_player) : string =
          let(np1, np2) : t_player * t_player = play_1_round(p1, p2)in 
        if (p1.hand) > [] && (p2.hand) > []
        then(
            
              print_two_players( play_1_round(p1, p2));
              wait(5);
              play_rounds(np1,np2)
          )
        else
        if (p1.hand) = []
        then "F�licitation joueur 2 tu as gagn�"
          else
          if (p2.hand) = []
          then "F�licitation joueur 1 tu as gagn�"
        else "erreur"
        ;;
          
        (*exercice 7*)
         (* Pour cet exercice, les fonctions permettant de r�aliser ces actions avaient d�ja �t� cr��es au pr�alable dans les autres exercices il a donc suffit de les appeler avec les parametres de play. *)

         let play (name1, name2 : string * string) : string =    
  print_two_players(init_play(name1,name2));
  play_rounds(init_play(name1,name2))
        ;;

        (*Deuxieme version jeu*)         
  (*exercice 8*)

         let rec play_1_round(p1, p2, cardlist : t_player * t_player * t_card list) : t_player * t_player =
          let c1 : t_card = fst(p1.hand) and c2 : t_card = fst(p2.hand) in
          let cardlist : t_card list = [] in
  if battle_value(c1) > battle_value(c2)
  then ((m_player(p1.name, add_lst(add_fst( rem_fst(p1.hand),c1),c2))), m_player(p2.name,(rem_fst(p2.hand)))
  add_lst(cardlist,c1););
  else
    if battle_value(c1) < battle_value(c2)
    then(( m_player(p1.name,(rem_fst(p1.hand)))),(m_player(p2.name, add_lst(add_fst(rem_fst(p2.hand),c2),c1)));
        add_lst(cardlist,c2)
        )
    else
      if battle_value(c1) = battle_value(c2)
      then( add_fst(cardlist,(p1.hand))
            add_fst(cardlist,(p2.hand))
            add_nth(cardlist,(p1.hand, 1))
            add_nth(cardlist,(p2.hand, 1))
            play_1_round
          )
        else
    ( (m_player(p1.name,add_lst(rem_fst(p1.hand)))),
      (m_player(p2.name,add_lst(rem_fst(p2.hand)))),
          rem_nth(cardlist,(p1.hand, 1)),
          rem_nth(cardlist,(p2.hand,1))
    
    )
;; 

 let rec play_rounds(p1, p2 : t_player * t_player) : string =
          let(np1, np2) : t_player * t_player = play_1_round(p1, p2)in 
        if (p1.hand) > [] && (p2.hand) > []
        then(
            
              print_two_players( play_1_round(p1, p2));
              wait(5);
              play_rounds(np1,np2)
          )
        else
        if (p1.hand) = []
        then "F�licitation joueur 2 tu as gagn�"
          else
          if (p2.hand) = []
          then "F�licitation joueur 1 tu as gagn�"
          else "erreur"
            
        ;;
