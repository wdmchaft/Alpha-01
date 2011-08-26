MCPFTSS ;WISC/MLH-PFT SPECIAL STUDIES ANCILLARY ;7/9/99  10:13
 ;;2.3;Medicine;**25,35**;09/13/1996
 ; Reference IA # 10061 for VADPT call.
 IF $G(DJR)=2452 D
 .  I V(23)="MECHANICS" S $P(DJJ(23),U,6)="MCPFT061"
 .  I V(23)="SMALL AIRWAY" S $P(DJJ(23),U,6)="MCPFT062"
 .  I V(23)="EXERCISE" S $P(DJJ(23),U,6)="MCPFT063"
 .  I V(23)="MAXIMUM PRESSURES" S $P(DJJ(23),U,6)="MCPFT064"
 .  Q
 ;END IF
 ;
 Q
RACECDE(DFN) ;Returns a value of RACE B = BLACK, O = ASIAN, and null for anything else
 D DEM^VADPT S MCRACE=$P(VADM(8),U,2)
 N MCMRACE,MCHOLD S MCHOLD=MCRACE,MCRACE=$$ETHN^MCPFTP1(MCHOLD,.VADM)
 D KVAR^VADPT S MCMRACE=""
 I MCRACE="" D RACEMSG
 I MCRACE'="" D
 .S:MCRACE["ASIAN" MCMRACE=MCMRACE_"O"
 .S:MCRACE["BLACK" MCMRACE=MCMRACE_"B"
 S MCRACE=$S(MCMRACE'="":MCMRACE,1:"")
 Q MCRACE
 ;
RACEMSG ; If patient race field is empty, notify user
 W $C(7),!!?5,"*** Patient's race not specified in the patient file ***"
 H 1 W $C(7) N X R X:3 W !
 Q
