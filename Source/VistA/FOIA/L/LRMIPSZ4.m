LRMIPSZ4 ;SLC/CJS/BA - MICRO PATIENT REPORT - AFB, FUNGUS ; 6/22/87  16:17 ;
 ;;5.2;LAB SERVICE;;Sep 27, 1994
TB ;from LRMIPSZ1
 I '$L($P(^LR(LRDFN,"MI",LRIDT,11),U)) Q:'$D(LRWRDVEW)  Q:LRSB'=11
 S LRTUS=$P(^LR(LRDFN,"MI",LRIDT,11),U,2),DZ=$P(^(11),U,5),LRAFS=$P(^(11),U,3),LRAMT=$P(^(11),U,4),Y=$P(^(11),U) D D^LRU
 W:LRHC ! W !,"* MYCOBACTERIOLOGY ",$S(LRTUS="F":"FINAL",LRTUS="P":"PRELIMINARY",1:"")," REPORT => "_Y_"   TECH CODE: "_DZ
 S LRPRE=23 D PRE^LRMIPSU
 K LRTA I $D(^LR(LRDFN,"MI",LRIDT,12,0)),$P(^(0),U,4)>0 S LRTA=0
 D:LRAFS'=""!($D(LRTA)) AFS
 I $D(^LR(LRDFN,"MI",LRIDT,13,0)),$P(^(0),U,4)>0 W:LRHC ! W !,"Mycobacteriology Remark(s):" S B=0 F I=0:0 S B=+$O(^LR(LRDFN,"MI",LRIDT,13,B)) Q:B<1  W !,?3,^(B,0)
 Q
AFS I LRAFS'="" W:LRHC ! W !,$S(LRAFS["D":"Direct",LRAFS["C":"Concentrate",1:"")," Acid Fast Stain:  ",$S(LRAFS["P":"Positive",LRAFS["N":"Negative",1:LRAFS),"   " W:$L(LRAMT) !,?3,"Quantity: ",LRAMT
 K ^TMP("LR",$J,"T"),LRTSTS I $D(LRTA) S LRTSTS=0 F A=0:1 S LRTA=+$O(^LR(LRDFN,"MI",LRIDT,12,LRTA)) Q:LRTA<1  S (LRBUG(LRTA),LRTBC)=$P(^(LRTA,0),U),LRQU=$P(^(0),U,2),LRTBC=$P(^LAB(61.2,LRTBC,0),U) D LIST
 Q
LIST W:LRHC ! W !,"Mycobacterium: ",LRTBC W:$L(LRQU) !,?3,"Quantity: ",LRQU S:$D(^LR(LRDFN,"MI",LRIDT,12,LRTA,2)) LRTSTS=LRTSTS+1
 I $D(^LR(LRDFN,"MI",LRIDT,12,LRTA,1,0)) W !,"   Comment: " S B=0 F I=0:0 S B=+$O(^LR(LRDFN,"MI",LRIDT,12,LRTA,1,B)) Q:B<1  W ?13,^(B,0),!
SEN S LRTB=2 F I=0:0 S LRTB=+$O(^LR(LRDFN,"MI",LRIDT,12,LRTA,LRTB)) Q:LRTB'["2."!(LRTB="")  S LRTBA=$O(^DD(63.39,"GL",LRTB,1,0)),LRTBA=$P(^DD(63.39,LRTBA,0),U),LRTBS=^LR(LRDFN,"MI",LRIDT,12,LRTA,LRTB) W !,?3,LRTBA,?20,LRTBS
 K LRTB,LRTBA,LRTBS
 Q
FUNG ;from LRMIPSZ1
 I '$L($P(^LR(LRDFN,"MI",LRIDT,8),U)) Q:'$D(LRWRDVEW)  Q:LRSB'=8
 S LRTUS=$P(^LR(LRDFN,"MI",LRIDT,8),U,2),DZ=$P(^(8),U,3),Y=$P(^(8),U) D D^LRU
 W:LRHC ! W !,"* MYCOLOGY ",$S(LRTUS="F":"FINAL",LRTUS="P":"PRELIMINARY",1:"")," REPORT => ",Y,"   TECH CODE: ",DZ
 S LRPRE=22 D PRE^LRMIPSU
 I $D(^LR(LRDFN,"MI",LRIDT,15)) W:LRHC ! W !,"MYCOLOGY SMEAR/PREP:" S LRMYC=0 F I=0:0 S LRMYC=+$O(^LR(LRDFN,"MI",LRIDT,15,LRMYC)) Q:LRMYC<1  W !?5,^(LRMYC,0)
 I $D(^LR(LRDFN,"MI",LRIDT,9,0)),$P(^(0),U,4)>0 W:LRHC ! W !,"Fungus/Yeast: " D SHOW
 I $D(^LR(LRDFN,"MI",LRIDT,10,0)),$P(^(0),U,4)>0 W:LRHC ! W !,"Mycology Remark(s):" S LRMYC=0 F I=0:0 S LRMYC=+$O(^LR(LRDFN,"MI",LRIDT,10,LRMYC)) Q:LRMYC<1  W !,?3,^(LRMYC,0)
 Q
SHOW S LRTA=0 F I=0:0 S LRTA=+$O(^LR(LRDFN,"MI",LRIDT,9,LRTA)) Q:LRTA<1  S (LRBUG(LRTA),LRTBC)=$P(^(LRTA,0),U),LRQU=$P(^(0),U,2),LRTBC=$P(^LAB(61.2,LRTBC,0),U) D LIST1
 Q
LIST1 W !,LRTBC W:$L(LRQU) !,?3,"Quantity: ",LRQU I $D(^LR(LRDFN,"MI",LRIDT,9,LRTA,1,0)) W !,?3,"Comment:" S B=0 F I=0:0 S B=+$O(^LR(LRDFN,"MI",LRIDT,9,LRTA,1,B)) Q:B<1  W ?13,^(B,0),!
 Q
