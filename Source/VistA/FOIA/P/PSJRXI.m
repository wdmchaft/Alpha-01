PSJRXI ;IHS/DSD/JCM/RLW-LOGS PHARMACY INTERVENTIONS ; 15 May 98 / 9:28 AM
 ;;5.0; INPATIENT MEDICATIONS ;**3,181**;16 DEC 97;Build 190
 ;
 ; Reference to ^APSPQA(32.4 is supported by DBIA #2179
 ; Reference to ^PSDRUG supported by DBIA# 2192
 ;
 ; This routine is used to create entries in the APSP INTERVENTION file.
 ;---------------------------------------------------------------
START ;   
 N SAVEX,SAVEY S SAVEX=X,SAVEY=Y
 D INIT
 D DIC G:PSJRXI("QFLG") END
 D EDIT
 S:'$D(PSJNEW("PROVIDER")) PSJNEW("PROVIDER")=$P(^APSPQA(32.4,PSJRXI("DA"),0),"^",3)
END D EOJ
 Q
 ;---------------------------------------------------------------
INIT ;
 W !!,"Now creating Pharmacy Intervention",!
 ;I $G(PSJDRUG("IEN")) W "For  ",$P($G(^PSDRUG(PSJDRUG("IEN"),0)),"^"),!
 I $G(PSJDD) W "For ",$P($G(^PSDRUG(PSJDD,0)),"^"),!
 K PSJRXI
 S PSJRXI("QFLG")=0
 Q
 ;
DIC ;
 N DIC,DR,DA,X,DD,DO,Y,PSJY,PSJRXIEN
 I $G(PSJRXREQ)="" S PSJRXREQ="ALLERGY"
 D FIND^DIC(9009032.3,"","@;.01","",PSJRXREQ,1,"B","","","PSJRXIEN")
 S PSJRXIEN=$G(PSJRXIEN("DILIST",2,1))
 I 'PSJRXIEN S PSJY=-1 G DICX
 S DIC="^APSPQA(32.4,",DLAYGO=9009032.4,DIC(0)="L",X=DT
 S DIC("DR")=".02////"_PSGP_";.04////"_DUZ_";.05////"_PSJDD_";.06///PHARMACY"
 ;S DIC("DR")=DIC("DR")_$S($G(PSJRX("INTERVENE"))=1:";.07////18",$G(PSJRX("INTERVENE"))=2:";.07////19",1:"")_";.14////0"_";.16////"_$S($G(PSJSITE)]"":PSJSITE,1:"")
 S DIC("DR")=DIC("DR")_";.07////"_$G(PSJRXIEN)_";.14////1"_";.16////"_$S($G(PSJSITE)]"":PSJSITE,1:"")
 D FILE^DICN K DIC,DR,DA
 S PSJY=Y
 I Y>0 S PSJRXI("DA")=+Y
 E  S PSJRXI("QFLG")=1 G DICX
 D DIE
DICX ;
 I $G(PSJY)=-1 D
 . W !!,"Unable to log an intervention for ",$G(PSJRXREQ)
 . D PAUSE^PSJLMUT1
 K X,Y
 Q
DIE ;
 K DIE,DIC,DR,DA
 S DIE="^APSPQA(32.4,",DA=PSJRXI("DA"),DR=$S($G(PSJRXI("EDIT"))]"":".03:1600",1:".03;.08")
 ;L +^APSPQA(32.4,PSJRXI("DA")) D ^DIE K DIE,DIC,DR,X,Y,DA L -^APSPQA(32.4,PSJRXI("DA"))
 L +^APSPQA(32.4,PSJRXI("DA")):1 E  W !,"Sorry, someone else is editing this intervention!" Q
 D ^DIE K DIE,DIC,DR,X,Y,DA L -^APSPQA(32.4,PSJRXI("DA"))
 W $C(7),!!,"See 'Pharmacy Intervention Menu' if you want to delete this",!,"intervention or for more options.",!
 Q
EDIT ;
 K DIR W ! S DIR(0)="Y",DIR("A")="Would you like to edit this intervention",DIR("B")="N" D ^DIR K DIR I $D(DIRUT)!'Y G EDITX
 S PSJRXI("EDIT")=1 D DIE
 G EDIT
EDITX K X,Y
 Q
 ;
EOJ ;
 K PSJRXI S X=SAVEX,Y=SAVEY
 Q
 ;
EN1(PSJORDER) ; Entry Point if have internal rx #
 I PSJX']"" W !,$C(7),"No prescription data" Q
 S PSJORDER=$S((PSJORDER["N")!(PSJORDER["P"):"^PS(53.1,"_+PSJORDER,PSJORDER["V":"^PS(55,"_DFN_",""IV"","_+PSJORDER,1:"^PS(55,"_DFN_",5,"_+PSJORDER)_","
 N PSJDFN,PSJNEW,PSJDRUG,PSJY
 I $G(^PS(53.1,PSJX,0))']"" W !,$C(7),"No prescription data" G EN1X
 S PSJRXI("IRXN")=PSJORDER
 K PSJY S PSJY=@(PSJORDER_",0)")
 S PSJDFN=$P(PSJY,"^",15),PSJNEW("PROVIDER")=$P(PSJY,"^",2)
 S PSJDRUG=0,PSJDRUG=$O(^PS(53.1,PSJRXI("IRXN"),1,PSJDRUG)) Q:'PSJDRUG  S PSJDRUG("IEN")=$G(@(PSJORDER_","_PSJDRUG),"^")
 D START
EN1X Q
