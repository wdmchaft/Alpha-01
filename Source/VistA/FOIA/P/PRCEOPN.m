PRCEOPN ;WISC/LDB-LIST OPEN 1358'S ;1/23/98  1230
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
OPN ;LIST OF OPEN (INCOMPLETE) 1358'S
 D EN^PRCSUT G W2^PRCSP1B:'$D(PRC("SITE")),EXIT^PRCSP1B:Y<0 S PRCSZZ=PRC("SITE")_"-"_PRC("FY")_"-"_PRC("QTR")_"-"_$P(PRC("CP")," ")
 S DHD="1358'S WITH OPEN AUTHORIZATIONS",L=0,DIC="^PRC(424,"
 S FLDS=".01,.05,.1",BY="[PRCEC OPN1358]"
 S DIS(0)="I $P($G(^PRC(424,D0,0)),""-"")=PRC(""SITE""),$P($G(^PRC(424,D0,0)),U,2),+$P($G(^PRC(442,$P(^(0),U,2),0)),U,3)=+PRC(""CP"")"
 S DIS(1)="S PRCSZZ1=$P($G(^PRC(442,$P(^PRC(424,D0,0),U,2),0)),U,12) I $P($P($G(^PRCS(410,PRCSZZ1,0)),U,1),""-"",1,4)=PRCSZZ"
 D EN1^DIP K PRCSZZ,PRCSZZ1,DHD,L,DIC,FLDS,BY,FR,TO Q
