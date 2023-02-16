# Base IMS DB


## 1- Introduction
Ce projet de démonstration présente la création d'une base IMS DB à partir d'un Modèle de Données ainsi que les programmes applicatifs COBOL permettant de réaliser les opérations courantes sur une base de données selon l’acronyme CRUD : « Creation », « Replace », « Update » et « Delete » de données dans la dB.

## Environnement 
•	Centrale IBM (Mainframe)

•	IMS DB

•	COBOL

•	JCL
## 2- Modèle de données
J’ai créé un modèle de données qui pourrait représenter la base de données d’un cabinet dentaire

(Pour info, le nom des entités est limité dans IMS à 8 caractères et je les ai nommés en anglais par commodité)

![](https://github.com/jeanmarcc/Base_IMS_DB/blob/90e19389f8b684ba5ce6cf963e128831054b80fa/DataModel.jpg)

## 3- Liste des Entités (« segments » au sens IMS DB) :

•	PATIENT

Représente un client du cabinet médical avec quelques propriétés comme son nom, prénom et date de naissance.
Cette entité est le segment « Root » de cette base de données IMS  

•	MEDICAL 

Entité représentant la visite d’un patient au cabinet médical. La principale propriété est la date  

•	TREATMNT

Contient les données de l’acte médical effectué pour le patient comme « détartrage », « extraction dentaire » …  ainsi que le praticien qui a réalisé l’acte

•	DRUG

Les médicaments prescrit pour le patient lors de l’acte médical avec le nom du médicament et la quantité prescrite

•	BILLING

Représente la facture pour la visite médicale dont l’attribut principal est le montant payé

•	PAYMENT

Entité qui représente le moyen de paiement utilisé par le patient pour régler l’acte comme « carte de crédit », « chèque » …

•	CONTACT

Contient des attributs additionnels du client comme son adresse postale

## 4- Liste des composants applicatifs  
    
Cette liste contient les definitions en assembler, les programmes Cobol applicatifs et les JCL associés    
    
| Objet 	| TYPE 		| Description 								 |
| ------------- | ------------- | ---------------------------------------------------------------------- |
| DENTDBD 	    | ASSEMBLER 	  | Description de la base de données IMS à partir de macros en assembler  | 
|               |               | Contient la déclaration des toutes les entités de la base,             |
|               |               | leur relation ainsi que leurs attributs                                |
| DENTPSB       | ASSEMBLER     | Description du PSB (Program Specification Block) utilisé par le        |
|               |               | programme d’insertion en masse de la base (« Initial Loading »)        |
| DENTPSBA      | ASSEMBLER     | Description du PSB utilisé par les programmes Cobol réalisant les      |
|               |               | opérations courantes sur la base (CRUD)                                |
| INITLOAD      | Cobol     	  | Programme Cobol pour charger en masse la base IMS (initial loading)  |
| JDBDGEN       | JCL     	    | JCL pour créer physiquement le DBD « DENTDBD »                         |
|         	    |       	      | Le compte rendu de ce JCL donne les attributs à utiliser pour créer  |
|         	    |       	      | les fichiers physiques de la db, par exemple :                       |
|         	    |       	      |                   *NOTE1              				                 |
|         	    |       	      |DEFINE CLUSTER (NAME(DENTET) -           				             |
|         	    |       	      |INDEXED KEYS (3,6) -              					                 |
|         	    |       	      |       RECORDSIZE (80,80)) 						                     |
|         	    |       	      |       DATA (CONTROLINTERVALSIZE (2048))                              |
| JPSBGEN/JPSBGENA        	|JCL      	|Permet de créer physiquement les PSB DENTPSB et DENTPSBA               |
|         	|       	|                                                                       |
| JHISAM        	|JCL       	|Permet de définir les fichiers physiques VSAM qui contiennent la       |
|         	        |       	|base IMS DB. Il y a un fichier principal de type KSDS et un fichier    |
|         	        |       	|overflow de type ESDS                                                      |
|JINITLOC/ JINITLOA         	|JCL        |Compilation du programme de chargement en masse de la base             |
|         	|       	|puis exécution                                                         |
|PCREA /PCREAJCL    |COBOL /JCL |Programme Cobol et JCL permettant de CREER des segments                |
|            	    |        	|dans la base IMS pour un patient                                        |
|             	    |         	|Les données des segments à créer sont dans un fichier séquentiel       |
|             	    |         	|en entrée du programme Cobol                                           |
|PREAD /PREADJCL    |COBOL /JCL | Programme Cobol et JCL permettant de LIRE des segments de la  |
|                	|         	| base IMS pour un patient                                      |
|PUPDA /PUPDAJCL    |COBOL /JCL | Programme Cobol et JCL pour Mettre A Jour des segments de la  |
|                	|          	| base IMS pour un patient                                      |
|                 	|          	|Les données des segments sont dans un fichier séquentiel en    |
|                 	|          	|entrée du programme Cobol                                        |
|PDELE /PDELEJCL    |COBOL /JCL | Programme Cobol et JCL pour Supprimer des segments de la base   |
|                	|         	| IMS pour un patient                                               |
|                 	|          	|Les données des segments sont dans un fichier séquentiel en        |
|                 	|          	|entrée du programme Cobol                                          |



## 5- Séquence des traitements 
La création de la base IMS et son chargement initial sont réalisés en respectant la séquence :
1.	JDBDGEN (création DBD)
2.	JPSBGEN (création du PSB pour le programme de chargement en masse)
3.	JPSBGENA (création du PSB pour les programmes applicatif CRUD)
4.	JHISAM (création des fichiers physiques VSAM qui contiendront la base IMS)
5.	JINITLOC + JINITLOA (compilation puis lancement du chargement en masse de la db à partir d'un fichier séquentiel à plat)

## 6- Fichiers en entrée
•	MATEBF.IMS.INITIAL.DATA.MYPROJ : pour chargement en masse, contient toutes le données (segments) à charger initialement

•	MATEBF.IMS.P*.FI01IN.MYPROJ : fichiers utilisés par les programmes applicatifs réalisant les opérations CRUD

## 7- Auteur
https://github.com/jeanmarcc
