set MOIS circular; #ensemble des mois
set PRODUITS; #ensemble des produits
set MACHINES; #ensemble des machines
set FABRIQUE dimen 2; #ensembles des couples (machine,produit) possibles

var stock{MOIS,PRODUITS} >= 0, integer; #les quantités conservées en fin de mois
var qte_prod{MOIS,PRODUITS} >= 0, integer; #les quantités produites par mois 
var vendre{MOIS,PRODUITS} >= 0, integer; #les quantités venudes par mois
#var choix{MOIS,MACHINES} >= 0 integer; # Q2 et Q3, choix des machines à mettre en maintenance
#var mach{MACHINES} >= 0 integer; # Q3, nombre de machines à acheter 

param nb_mach{MACHINES} >= 0; #nombre de machines par défaut
param maint{MOIS,MACHINES} >= 0; #planning de maintenance par défaut
param vente_max{MOIS,PRODUITS} >= 0; #capacités de ventes maximales
param prix{PRODUITS} > 0; #prix de vente unitaire pour chaque produit
param heures_fabrique{FABRIQUE} > 0; #temps d'utilisation d'une machine pour fabriquer un produit
param nb_jours; #nombre de jours ou l'usine fonctionne dans un mois 
param nb_heures; #nombre d'heures ou l'usine fonctionne dans une journée 
param cout_stock; #coût du stockage d'une unité de produit 
param stock_max; #capacité de stockage maximale
param stock_dep; #stock initial
param stock_fin; #stock à la fin du mois de juin
param achat{MACHINES} >= 0; # Q3 achat de machines, prix unitaire des machines

maximize profit: #fonction à maximiser
	sum{m in MOIS, p in PRODUITS} vendre[m,p]*prix[p] -sum{m in MOIS, p in PRODUITS} stock[m,p]*cout_stock;


#maximize profit: # Q3 achat de machines, nouvelle fonction à maximiser
#	sum{m in MOIS, p in PRODUITS} vendre[m,p]*prix[p] -sum{m in MOIS, p in PRODUITS} stock[m,p]*cout_stock-sum{k in MACHINES}mach[k]*achat[k];


subject to contrainte_vente_max {m in MOIS, p in PRODUITS}: #ne pas dépasser les capacités de ventes maximales 
	vendre[m,p] <= vente_max[m,p];

subject to contrainte_stock_max {m in MOIS, p in PRODUITS}: #ne pas dépasser la capacité de stockage maximale
	stock[m,p]<= stock_max;

subject to contrainte_conservation_stock {m in MOIS, p in PRODUITS}: #équilibre entre les variables de stockage, vente et production
	qte_prod[m,p] = stock[m,p]-(if m != "Janvier" then stock[prev(m),p] else stock_dep)+vendre[m,p];

subject to contrainte_stock_fin {p in PRODUITS}: #on veut fixer les stocks à la fin du mois de juin
	stock["Juin",p] = stock_fin;

################## Question 1 ##################
subject to contrainte_disponibilite1 {m in MOIS, k in MACHINES}:	 #contrainte pour ne pas dépasser le temps d'utilisation maximum des machines
	sum{(k,p) in FABRIQUE} heures_fabrique[k,p]*qte_prod[m,p] <= nb_heures*nb_jours*(nb_mach[k]-maint[m,k]);

################## Question 2 ##################
#subject to contrainte_disponibilite2 {m in MOIS, k in MACHINES}:	#contrainte pour ne pas dépasser le temps d'utilisation maximum des machines
#	sum{(k,p) in FABRIQUE} heures_fabrique[k,p]*qte_prod[m,p] <= nb_heures*nb_jours*(nb_mach[k]-choix[m,k]);

#subject to contrainte_maint21 {k in MACHINES : k != "bro"}: #toutes les machines sauf les broyeuses doivent être en maintenance
#	sum{m in MOIS} choix[m,k] = nb_mach[k];

#subject to contrainte_maint22 : #deux des quatre broyeuses doivent être en maintenance
#	sum{m in MOIS} choix[m,"bro"]=2;

################## Question 3 ##################
#subject to contrainte_disponibilite3 {m in MOIS, k in MACHINES}:	#contrainte pour ne pas dépasser le temps d'utilisation maximum des machines
#	sum{(k,p) in FABRIQUE} heures_fabrique[k,p]*qte_prod[m,p] <= nb_heures*nb_jours*(nb_mach[k]+mach[k]-choix[m,k]);

#subject to contrainte_maint31 {k in MACHINES : k != "bro"}: #toutes les machines sauf les broyeuses doivent être en maintenance
#	sum{m in MOIS} choix[m,k] = nb_mach[k]+mach[k];

#subject to contrainte_maint32 : #toutes les broyeuses sauf deux doivent être en maintenance
#	sum{m in MOIS} choix[m,"bro"]=2+mach["bro"];
