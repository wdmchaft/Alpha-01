HBHCDIS ; LR VAMC(IRMS)/MJT-HBHC Discharge data entry, calls STATUS^HBHCUTL from [HBHC DISCHARGE] template ;9202
 ;;1.0;HOSPITAL BASED HOME CARE;**2**;NOV 01, 1993
START ; Initialization
 S HBHCFORM=5
PROMPT ; Prompt user for patient name
 K DIC,HBHCPRCT S DIC="^HBHC(631,",DIC(0)="AEMQZ" D ^DIC
 G:Y=-1 EXIT
 I $P(Y(0),U,15)'=1 W *7,!!,"Record indicates patient has not been admitted to HBHC.  Discharge not allowed",!,"without admission.",! H 3 G PROMPT
 S HBHCDFN=+Y,HBHCDPT=$P(Y,U,2),HBHCXMT5=$P($G(^HBHC(631,HBHCDFN,1)),U,18)
 I (HBHCXMT5]"")&(HBHCXMT5'="N") D FORMMSG^HBHCUTL1 G:$D(HBHCNHSP) EXIT G:HBHCPRCT'=1 PROMPT
 K DIE S DIE="^HBHC(631,",DA=HBHCDFN,DR="[HBHC DISCHARGE]"
 L +^HBHC(631,HBHCDFN):0 I $T D ^DIE L -^HBHC(631,HBHCDFN) G PROMPT
 W *7,!!,"Another user is editing this entry.",!! G PROMPT
EXIT ; Exit module
 K DA,DIC,DIE,DR,HBHC12,HBHC359,HBHCDFLG,HBHCDFN,HBHCDIED,HBHCDPT,HBHCFORM,HBHCI,HBHCJ,HBHCL,HBHCM,HBHCNHSP,HBHCNOD1,HBHCPRCT,HBHCQ,HBHCQ1,HBHCTFLG,HBHCXMT5,HBHCWRD1,HBHCWRD2,HBHCWRD3,HBHCY0,X,Y
 Q
