# kHorn
Analyse des formes normales de Horn et recherche d'algorithmes polynomial y répondant.

## Sujet
Vous pouvez trouver le sujet à la racine dans `kHorn.pdf`.

## Rapport
Pour lire le rapport lancez 
```bash
make rapport
```
## Algorithmes
Pour compiler le programme testant la satisfiabilité d'une formule de Horn lancez
```bash
make
```
ou
```bash
make main
```

Pour l'executer il suffit ensuite de faire `./main <nom_du_test>`.

Pour compiler le programme testant si une forule est possiblement transformable en forme normale de Horn, vous pouvez lancer 
```bash
make converter
```
(PSA: le programme peut ne pas accepter une formule de la forme x \/ x car je n'ai pas implémenté la suppression des doublons qui suffirait pour obtenir la véritable équivalence)

## Tests
Pour lancer un test faites `./main <nom_du_test>`. Si vous voulez lancer tous les tests (sauf ceux ne compilant pas) en même temps il suffit de faire 
```bash
make all_tests
```
