const NBPASMAX = 151 #= Nombre maximal de pas de temps =#
const NBHORMAX = 60 #= Nombre maximal d'horizons de sol =#
const MAXLINE = 250  #= Longueur maxi de la ligne dans fichiers texte =#
const NBCASEMAX = 301  #= Nombre maximal de cases de sol en X, Y et Z =#
const CONVERROR = 1000  #= Mximal number of trials allowed when truing to converge in functions =#

const filename = "parameter.xml"
const deltaT=1         #= Time steps, in days =#
const epsilon=1.0e-8 #= Small value, close to 0 =#
const epaissHor=50.0  #= Epaisseur horizons de sol (en mm) =#
const longSegNorm=5.0  #= Longueur habituelle des segments form?s (mm) =#
const longSegMin=2.0  #= Longueur minimale des segments form?s, quand faible croissance (mm) =#
const dureeSansCreation=1.9 #= Dur?e maximale sans cr?ation de noeud, quand faible croissance (jour) =#
const mailleMin=6.0  #= Valeur de la maille minimale de sol (mm) =#
const d1=2.0   #= Premi?re valeur de distance (mm) =#
const d2=4.0  #= Deuxi?me valeur de distance (mm) =#
const d3=8.0  #= Troisi?me valeur de distance (mm) =#
const d4=16.0 #= Quatri?me valeur de distance (mm) =#
const d5=32.0  #= Cinqui?me valeur de distance (mm) =#

#= Param?tres, lus dans fichier param?tres =#
P_duree::Int32 = 70 #= Dur?e de la simulation, en jours =#


# Export
P_exportType::Int32 = 1 #= 1 = txt, 2 = RSML =#
P_exportName = "myroot" #= Name of the exported file =#
P_exportName2 = "myroot" #= Name of the exported file =#
P_exportExt = ".txt" #= Name of the extension =#

# Caract?risation de l'?mission des racines primaires
P_vitEmissionSem::Float32=0.5 #= Vitesse d'?mission des primaires (en jour-1) =#
P_nbMaxSem::Float32=1 #= Nombre maximal de racines primaires =#
P_propDiamSem::Float32=1.0 #= Proportion du diam?tre des s?minales (par rapport au diam?tre max) =#
P_angInitMoyVertSem::Float32=0.7854 #= Angle d'insertion moyen par rapport ?? la verticale pour les primaires =#
P_angInitETVertSem::Float32=0.35  #= ?cart-type de l'angle d'insertion des primaires =#

# Caract?risation de l'?mission des adventives
P_ageEmissionAdv::Float32=12.0 #= ??ge de commencement de l'?mission des racines adventives =#
P_vitEmissionAdv::Float32=2.0 #= Vitesse d'?mission des adventives (en jour-1) =#
P_dBaseMaxAdv::Float32=30.0 #= Distance ?? la base maximale pour les adventives (mm) =#
P_propDiamAdv::Float32=1.0 #= Proportion du diam?tre des adventives (par rapport aux diam?tre max) =#
P_nbMaxAdv::Int32=40 #= Nombre maximal de racines adventives =#

P_angInitMoyVertAdv::Float32=1.4 #= Angle d'insertion moyen par rapport ?? la verticale pour les adventives =#
P_angInitETVertAdv::Float32=0.7  #= ?cart-type de l'angle d'insertion des adventives =#

# Croissance radiale
P_coeffCroissRad::Float32=0.6 # coefficient de croissance radiale

# Allongement (croissance axiale)
P_diamMin::Float32=0.10  #= Diam?tre minimal en deça duquel il n'y a pas de croissance (mm) =#
P_diamMax::Float32=1.1   #= Diam?tre maximal donn? aux racines primaires (mm) =#
P_penteVitDiam::Float32=12.0 #= pente de la relation entre vitesse de croissance et diam?tre (mm.mm.jour-1) =#
P_tendanceDirTropisme::Int32=2  #= Type de tropisme (0: plagio -1: geo- +1: geo+ 2: exo =#
P_intensiteTropisme::Float32=0.2 #= Coefficient multipli? par le diam?tre pour chaque racine =#
P_penteDureeCroissDiam2::Float32=3000.0 #= pente de la relation dur?e de croissance versus diam?tre^2 =#

# Ramification
P_ageMaturitePointe::Float32=4.5  #= ?ge de maturit? des m?rist?mes (jours) =#
P_distRamif::Float32=4.0 #= distance inter-ramification (mm) =#
P_probEmergeDmax::Float32=0.8  #= probabilit? d'?mergence d'une lat?rale sur un axe de diam Dmax =#
P_probEmergeDmin::Float32=0.0  #= probabilit? d'?mergence d'une lat?rale sur un axe de diam Dmin =#
P_propDiamRamif::Float32=0.2 #= proportion de diam?tre des filles par rapport à leur m?re =#
P_coeffVarDiamRamif::Float32=0.30 #= coefficient de variation du diam?tre des ramifs =#
P_angLat::Float32=1.3 #= angle d'insertion des racines lat?rales =#

# Mortalit?
P_TMD::Float32=0.2 #= Tissue mass density, ou masse volumique =#
P_penteDureeVieDiamTMD::Float32=2000.0 #= pente de la relation dur?e de vie versus diam?tre et TMD =#

#= Variables globales diverses =#
#global temps::Int32=0  #= Le temps, en jours =#
orig = Array{Float32}(undef, 3)      #= Position d'origine du syst?me racinaire =#

vox = Array{Int8}(undef, NBCASEMAX+1, NBCASEMAX+1, NBCASEMAX+1)  #= tableau sol-voxel dont les cases vont contenir des entiers entre 1 et 6 =#

maille::Float32=mailleMin #= Valeur initialis?e de la maille de sol =#
volElemSol::Float64 = 0  #= Volume ?l?mentaire de sol associ? à la maille (mm3) =#

