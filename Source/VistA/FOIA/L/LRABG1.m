LRABG1 ;SLC/RWF - PULMONARY LAB DATA DISPLAY ;2/22/87  2:08 PM ;
 ;;5.2;LAB SERVICE;**187,201,213**;Sep 27, 1994
PRL K LRXCH S:IOST["P-" LRXCH=1 D:$Y+4>IOSL WAIT,HEAD:'LREND S LRIDT=$O(^LR(LRDFN,"CH",LRIDT)) G LREND:LRIDT="",LREND:LREND I $G(LREDT),LRIDT>LREDT G LREND
 G PRL:$O(^LR(LRDFN,"CH",LRIDT,443))=""!($O(^(443))>459) S Z=^(0) G PRL:'$P(Z,U,3) I LRTOP>0 G:LRTOP'=$P(Z,U,5) PRL
 S Z6="" F I=444,446:1:449,451:1:459 S Z6=Z6_$S($D(^LR(LRDFN,"CH",LRIDT,I)):$P(^(I),"^",1),1:"")_"^"
 S Z2=$S($D(^LR(LRDFN,"CH",LRIDT,445)):^(445),1:"")
 S Z8=$S($D(^LR(LRDFN,"CH",LRIDT,450)):^(450),1:"")
 S Z=^LR(LRDFN,"CH",LRIDT,0),X=$P(Z,U,1) D DAT
 W ! W:Y'=LRLDT Y
 W !?2,"@"_T S LRSPEC=$P(Z,U,5),LRLDT=Y W $E("AVC  W",LRSPEC-79),?11
 S LRZZ=$P(Z2,U,1),I=1 X LRXW
 S I=2,LRZZ=$P(Z6,U,I) X LRXW S I=3,LRZZ=$P(Z6,U,I) X LRXW S I=4,LRZZ=$P(Z6,U,5) X LRXW
 S LRZZ=$P(Z8,U,1),I=5 X LRXW
 F I=6:1:9 S LRZZ=$P(Z6,U,I) X LRXW
FI W ?75 S LRFIO2=$P(Z6,U,1) W LRFIO2 IF LRFIO2["L" S LRFIO2=LRFIO2*4+20 W "=",LRFIO2,"%"
 W ! I $L($P(Z,U,5)),$D(^LAB(61,$P(Z,U,5),0)) W $E($P(^(0),U,1),1,14)
 IF $P(Z6,U,4)>1 W ?15,"%MetHb IS ",$P(Z6,U,4)
 S LRPT=$P(Z6,U,11) IF LRPT,LRPT'=37 W ?26,"TEMP ",LRPT,": " F K=12:1:14 S LRZZ=$P(Z6,U,K),I=K-7 X LRXW
 IF $D(^LR(LRDFN,"CH",LRIDT,1)) W !,?6,"NOTE:" S I=0 F  S I=$O(^LR(LRDFN,"CH",LRIDT,1,I)) Q:I<1  W ^(I,0),!
 D AA G PRL
AA S LRPCO2=$P(Z6,U,6),LRPO2=$P(Z6,U,7) ;DIF=AGE*.28-3.06
 IF $P(Z6,U,11)-37 S X=$P(Z6,U,13),Y=$P(Z,U,14) S:X>1 LRPCO2=X S:Y>1 LRPO2=Y
 IF LRSPEC-80!(LRFIO2["CA") W ! Q  ;SPEC'=ART. BLOOD
 S LRFIO2=LRFIO2/100,LRALV=600*LRFIO2-(LRPCO2*(LRFIO2+(1-LRFIO2/.79)))
 W !?6,"computed LRALV-art=",$J(LRALV-LRPO2,1,0) W:LRALV<LRPO2 " ERROR,",$C(7)
 IF LRALV S X=$J(LRPO2/LRALV,1,2) W "  art/LRALV=",X W:X'>.75 " (ratio should be above .75)" W:X>1 " ERROR",$C(7)
 W ! Q
HEAD ;from LRABG
 W @IOF S LRLDT=0,X=DT D DAT
 W !,$P($G(^DIC(4,+$P($G(^XMB(1,1,"XUS")),U,17),0)),U)," BLOOD GAS REPORT",?60,Y
 W !?5,SSN,?30,PNM,?60,"AGE ",AGE
 W !,"DATE    A/V"
H4 F I=1:1:3 W ! F J=0:1:10 W:J=0 $S(I=1:"    TIME",I=2:"Ref High",1:"Ref Low ") I J>0 W $S($D(LRLN(J)):$J($P(LRLN(J),U,I),7),I=1:$J($P(^LAB(60,$P(LRTST,U,J),.1),U,1),7),1:"       ")
 W ! F J=1:4:76 W "----"
 W "---" Q
DAT S Y=$$FMTE^XLFDT(X,"5ZM")
 S T=$P(Y,"@",2),Y=$P(Y,"@") Q
 ;
WAIT Q:$D(LRXCH)  S LREND=0 R !,"PRESS '^' TO STOP ",J:DTIME U IO
 S:J="" J=1 S LREND=".^"[J Q
LREND W:'LREND !," last blood gas" D:LRIDT<1 WAIT D ^%ZISC
 K LRIDT,LRXW,LRPQ,LRPJ,LRFIO2,LRPO2,LRALV,LRDFN,LRDPF,LRLDT,LRLI
 K LRLL,LRLLT,LRLN,LRLNM,LRLO,LRLOC,LRPCO2,LRPT,LRSPEC,LRTOP,LRTST
 K LRUTLITY,LRXCH,LRZZ,PNM,SEX,SSN,T,Z,Z2,Z6,Z8,I,J
 Q
