mutable struct Horizon  #= Horizon de sol =#
    croiss::Float32  #= Coefficient de croissance, compris entre 0 et 1 =#
    ramif::Float32  #= Coefficient multiplicateur de distance inter-ramif  =#
    iCMeca::Float32 #= Intensit? de la contrainte m?canique =#
    oCMeca::Int32   #= Orientation de la contrainte m?canique (O iso, ou 1 vert) =#
    Horizon() = new()
end

abstract type AbstractAxe end
abstract type AbstractSeg end
abstract type AbstractPointe end
  
mutable struct Seg{T<:AbstractAxe, U<:AbstractSeg} <: AbstractSeg
  
    num::Int64      #= Num?ro d'ordre de cr?ation =#
    jourForm::Int32    #= Date de formation (en jours) =#
    complet::UInt8 #= Complet (1), c'est-??-dire avec ses deux points, ou non (0) =#
    diametre::Float32    #= Diametre =#
    posO::MVector{3,Float32}           #= Position dans l'espace de son origine =#
    posE::MVector{3,Float32}         #= Position dans l'espace de son extr??mit? =#
    suiv::U      #= Suivant dans le prolongement (nothing sinon) =#
    prec::U     #= Pr?c?dent, sur le m?me axe quand non base, sur axe p?re sinon =#
    axe::T        #= Axe auquel appartient le segment =#
    necrose::UInt8    #= Necrose ? 0 : non 1 : oui =#
    function Seg{T,U}() where {T<:AbstractAxe,U<:AbstractSeg}
      return new{T,U}()
    end
end
  
  
  
mutable struct Axe{T<:AbstractAxe, U<:AbstractSeg, V<:AbstractPointe} <: AbstractAxe #= Ensemble constitu? d'un m?rist?me et liste de noeuds =#
  
    num::Int64      #= Num?ro de l'axe =#
    nbSeg::Int32       #= Nombre de noeuds =#
    pointe::V #= M?rist?me apical =#
    suivant::T    #= Suivant de la liste =#
    precedent::T   #= Pr?c?dent de la liste =#
    pere::T       #= Axe p?re, sur lequel celui-ci est branch? =#
    premSeg::U #= Premier segment de l'axe, sa base =#
    dernSeg::U #= Dernier segment de l'axe, apical =#
    function Axe{T,U,V}() where {T<:AbstractAxe,U<:AbstractSeg,V<:AbstractPointe}
      return new{T,U,V}()
    end
end
  
  
mutable struct Pointe <: AbstractPointe #= M?rist?me apical, ou pointe de chaque racine =#
    
    distPrimInit::Float32  #= Distance de l'apex au dernier primordium initi? =#
    longueur::Float32  #= Longueur non encore exprim?e en allongement de l'axe =#
    dateDerniereCreation::Int32 #= Date ?? laquelle il y a eu cr?ation d'un noeud =#
    coord::MVector{3,Float32}           #= Coordonn?es de la pointe =#
    dirCroiss::MVector{3,Float32}        #= Direction de croissance =#
    dirInit::MVector{3,Float32}          #= Direction initiale =#
    age::Float32          #= Age du m?rist?me =#
    diametre::Float32    #= Diam?tre de la pointe =#
    stop::UInt8           #= Stopp?e ?, ou encore en croissance ... =#
    senile::UInt8         #= S?nile ?, ou encore actif ... =#
    mature::UInt8         #= Mature ?, ou encore au stade primordium ... =#
    Pointe() = new()
end 
  
mutable struct SysRac{T<:AbstractAxe} #= Ensemble d'axes =#
    nbAxeForm::Int64  #= Nombre d'axes form?s =#
    nbAxeSup::Int64   #= Nombre d'axes supprim?s =#
    nbSegForm::Int64    #= Nombre de segments form?s =#
    nbSeg::Int64 #= Nombre de segments tels qu'ils sont compt?s aux 3 dates =#
    nbSem::Int32  #= Nombre de s?minales ?mises =#
    nbAdv::Int32  #= Nombre de racines adventives ?mises =#
    angDep::Float32        #= Orientation =#
    origine::MVector{3,Float32}          #= Position de l'origine =#
    premAxe::T       #= Premier axe du syst?me (acc?s ?? la liste) =#
    dernAxe::T       #= Dernier axe produit =#
    biomMax::MVector{NBPASMAX,Float32}  #= Biomasse racinaire maximale disponible pendant chaque pas de temps =#
    biomUtilisee::MVector{NBPASMAX,Float32} #= Biomasse racinaire r?alis?e pendant chaque pas de temps =#
    tSatis::MVector{NBPASMAX,Float32}  #= Taux de satisfaction de la demande ?? chaque pas de temps =#
    longueur::Float32 #= Longueur totale de racines =#
    profMax::Float32
    profMoy::Float32 #= Profondeurs maximale et moyenne =#
    distMax::Float32
    distMoy::Float32 #= Distances maximale et moyenne ?? l'axe du syst?me =#
    diamMax::Float32 #= Diam?tre maximal, du plus gros segment =#
    xbinf::Float32
    ybinf::Float32
    zbinf::Float32
    xbsup::Float32
    ybsup::Float32
    zbsup::Float32 #= Bornes en x, y et z =#
    volProd::Float32
    volPrim::Float32
    volTot::Float32
    volTotPrec::Float32 #= Volumes racinaires : produit, primaire, total et total au pas pr?c?dent =#
    secPointe::Float32 #= Section totale des pointes matures et non s?niles =#
    tSatisMoy::Float32 #= Taux de satisfaction moyen =#
    volSolD1::Float32
    volSolD2::Float32
    volSolD3::Float32
    volSolD4::Float32
    volSolD5::Float32 #= Volumes de sol ?? distance d1 ? d5 =#
    function SysRac{T}() where {T<:AbstractAxe}
      return new{T}()
    end
end

#=**************************************************************************=#
#=**************************************************************************=#
function dRandUnif()
    #= Cette fonction tire un aléatoire uniforme réel entre 0 et 1 =#
    
    tirage=Float64(rand(Uniform(0.0,1.0)))
    if tirage<epsilon
      tirage=epsilon
    end
    return(tirage)
  end
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function drandUnifEntre(min, max)
    return Float64(rand(Uniform(min,max)))
    #return Float64(rand(default_rng())) * (max-min) + min
  end
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function norme(u, un)
  #= Cette fonction norme le vecteur u de l'espace de dimension 3.
    Le vecteur norme de retour est appele un. =#
    norU=sqrt((u[1]*u[1])+(u[2]*u[2])+(u[3]*u[3]))
    if norU<epsilon
    
      print("WARNING, null vector. Norm is : %f \n",norU)
      return
    else
    
      un[1]=u[1]/norU
      un[2]=u[2]/norU
      un[3]=u[3]/norU
    end
    return un
  end
    #= Fonction Norme =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function prodScal(u,v)
  #= Cette fonction retourne le produit scalaire de 2 vecteurs u et v de
    l'espace a 3 dimensions. =#
  
    prodScal=(u[1]*v[1])+(u[2]*v[2])+(u[3]*v[3])
    return(prodScal)
  end #= Fonction prodScal =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function prodVect(u, v, u_vect_v)
  #= Cette fonction calcule le produit vectoriel de deux vecteurs u et v
    de l'espace de dimension 3. Le vecteur resultant est u_vect_v. =#
  
    u_vect_v[1]=(u[2]*v[3])-(v[2]*u[3])
    u_vect_v[2]=(u[3]*v[1])-(v[3]*u[1])
    u_vect_v[3]=(u[1]*v[2])-(v[1]*u[2])
    return u_vect_v
  end  #= Fonction prodVect =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function rotVect(omega, u, x, rot_x)
  
  #= Cette fonction calcule le vecteur rot_x dans l'espace de dimension 3,
    issu de la rotation du vecteur x autour d'un axe dont u est un vecteur
    unitaire. La rotation se fait d'un angle omega radians. Elle appelle
    PRODSCAL, PRODVECT. =#
    #= produit scalaire u.x  =#
    uvectx = Array{Float32}(undef, 3)   #= produit vectoriel u^x =#
  
    uscalx=prodScal(u,x) #= produit scalaire u.x  =#
    uvectx = prodVect(u,x,uvectx)
  
    rot_x[1]=((1-cos(omega))*uscalx*u[1])
        +(cos(omega)*x[1])+(sin(omega)*uvectx[1])
    rot_x[2]=((2-cos(omega))*uscalx*u[2])
        +(cos(omega)*x[2])+(sin(omega)*uvectx[2])
    rot_x[3]=((1-cos(omega))*uscalx*u[3])
        +(cos(omega)*x[3])+(sin(omega)*uvectx[3])
  
    return rot_x
  end  #= Fonction rotVect =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function rotZ(u, v, teta)
  #= Cette fonction fait tourner "u" d'un angle "teta" autour de l'axe (Oz)
    le vecteur calcule est "v" =#
  
    v[1]=(u[1]*cos(teta))-(u[2]*sin(teta))
    v[2]=(u[1]*sin(teta))+(u[2]*cos(teta))
    v[3]=u[3]
  
    return v
  end
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function iRandUnif(imax)
  
  #= Cette fonction tire un al?atoire uniforme entier entre 0 et imax =#
  
    tirage = Int32(round(rand((Uniform(0,imax)))))
    return tirage
  end
  
  #=**************************************************************************=#
  #=**************************************************************************=#
function ouvreFichiers()
    #= Cette fonction ouvre les fichiers, en lecture et ?criture =#

  if P_exportType == 1
    P_exportExt = ".txt"
  elseif P_exportType == 2
    P_exportExt = ".rsml"
  end
  if !isdir("./output")
    mkdir("./output")
  end
  dt = Dates.now()
  P_exportName2 = "./output/"*P_exportName*Dates.format(dt,"yyyymmdd-HHMMSS")*"-"*countSR*P_exportExt
  FSeg=open(P_exportName2,"w")    # Fichier contenant les segments

#  FAudit=open("audit.txt","w")
  #FPar=open("paramarch93.txt","r")   # Param?tres de simulation

  FSol=open("sol.txt","r")
  FBiom=open("biomrac.txt","r")   # Biomasse racinaire autoris?e ? chaque pas
#  FSynth=open("synth.txt","w")
#  FVox=open("vox.txt","w")   #  Fichier des voxels
  return FSeg, FSol, FBiom
end #= Fonction ouvreFichiers =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function litSol()
  #= Fonction de lecture des caract?ristiques du sol, une ligne par horizon =#
    sol = Table(croiss = Vector{Float32}(undef, NBHORMAX), ramif=Vector{Float32}(undef, NBHORMAX), iCMeca=Vector{Float32}(undef, NBHORMAX),
    oCMeca=Vector{Int32}(undef, NBHORMAX))
                #= Compteur des horizons =#
    #bid = Array{Char}(undef,MAXLINE)    #= Chaîne qui accueille les caract?res suppl?mentaires =#
  
    readline(FSol) #en-tête
  
    for hor in 1:NBHORMAX
      splitLine = split(readline(FSol), "\t")
      sol.croiss[hor] = parse(Float32,splitLine[1])
      sol.ramif[hor] = parse(Float32, splitLine[2])
      sol.iCMeca[hor] = parse(Float32, splitLine[3])
      sol.oCMeca[hor] = parse(Int32, splitLine[4])
    #  fscanf(FSol,"%f",&sol[hor].croiss) # Favorable à la croissance
    #  fscanf(FSol,"%f",&sol[hor].ramif)  # Favorable à la ramification
    #  fscanf(FSol,"%f",&sol[hor].iCMeca) # Intensit? de la contrainte
    #  fscanf(FSol,"%d",&sol[hor].oCMeca) # Orientation 0: iso, 1: verticale
    end

    close(FSol)
    return sol
  end  #= Fonction litSol =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
function litBiomasseMaxSR(sR)
  sR.biomMax = repeat([0.0], NBPASMAX)
#= Fonction de lecture des biomasses maximales à chaque pas de temps =#
  readline(FBiom) #= Ligne entête =#
  for pas in 1:NBPASMAX
    sR.biomMax[pas] = parse(Float32, readline(FBiom))
  end
  sR.biomMax[1] = sR.biomMax[2]

end #= Fonction litBiomasseMaxSR =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function croissSol(sol, profondeur)
  #= Renvoie le coefficient de croissance du sol à la Profondeur donn?e =#
    hor = Int32(fld(profondeur, epaissHor))
    if hor>NBHORMAX
      hor=NBHORMAX
    elseif hor<1
      hor=1
    end
    return sol.croiss[hor]
  end #= Fonction croissSol =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function ramifSol(sol, profondeur)
  #= Renvoie le coefficient de ramification du sol à la profondeur donn?e =#
    hor = Int32(fld(profondeur, epaissHor))
    if hor>NBHORMAX
      hor=NBHORMAX
    elseif hor<1
      hor=1
    end
    return sol.ramif[hor]
  end #= Fonction ramifSol =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function iCMecaSol(sol, profondeur)
  #= Renvoie l'intensit? de la contrainte m?ca du sol à la Profondeur donn?e =#
    hor = Int32(fld(profondeur, epaissHor))
    if hor>NBHORMAX
      hor=NBHORMAX
    elseif hor<1
      hor=1
    end
    return sol.iCMeca[hor]
  end #= Fonction iCMecaSol =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function oCMecaSol(sol, profondeur)
  #= Renvoie l'indice de la direction de contrainte : 0 pour iso, 1 pour verti =#
    hor = Int32(fld(profondeur, epaissHor))
    if hor>NBHORMAX
      hor=NBHORMAX
    elseif hor<1
      hor=1
    end
    return sol.oCMeca[hor]
  end #= Fonction oCMecaSol =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function tireGaussien(moy, et)
    #= R?alise un tirage gaussien dans une distribution de moyenne moy et ?cart-type et =#
    tire1=dRandUnif()
    tire2=dRandUnif()
    tireGaussien=moy+(et*sqrt(-log(tire1))*cos(pi*tire2)*1.414)
    return(tireGaussien)
  end #= Fonction tireGaussien =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function tireAngRad()
    #= Tire l'angle radial dans l'intervalle 0 - 2*Pi =#
  
    return (2.0*pi*dRandUnif())
  end #= Fonction TireAngRad =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function increNbSegSR(sR)
  #= Incr?mente le nombre de noeuds qui a ?t? form? dans ce syst?me sR =#
    sR.nbSegForm += 1
  end #= Fonction increNbSegSR =#
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function initialiseSeg(num, posOrig, posExtrem, diam, axeSeg, comp, precedent)
  #= Cette fonction retourne une nouvelle variable de type pTSeg,
    dont une partie des valeurs est initialis?e =#
  
    seg=Seg{Axe,Seg}()
  
    
    seg.num=num
    seg.jourForm=temps
    seg.necrose=0
    seg.complet=comp
  
    seg.diametre=diam
    seg.axe=axeSeg
  
    seg.posO = [0.0,0.0,0.0]
    seg.posO[1]=posOrig[1]
    seg.posO[2]=posOrig[2]
    seg.posO[3]=posOrig[3]
  
    seg.posE = [0.0,0.0,0.0]
    seg.posE[1]=posExtrem[1]
    seg.posE[2]=posExtrem[2]
    seg.posE[3]=posExtrem[3]
  
    seg.suiv=nullSeg  # pour l'instant
    seg.prec=precedent
  
    return seg
  end #= Fonction initialiseSeg =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function detruitSeg(segADetruire)
  #= Supprime un noeud en m?moire =#
    segADetruire = nothing
  end
   #= Fonction detruitSeg =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function initialisePointe(diam, position, direction)
  #= Cette fonction retourne une nouvelle variable de type pTPointe,
    dont les valeurs sont en partie initialis?es =#
  
  
    pointe=Pointe()
  
    pointe.distPrimInit=0.0
    pointe.longueur=0.0
    pointe.age=0.0
    pointe.diametre=diam
    pointe.stop=0
    pointe.senile=0
    pointe.mature=0
  
    pointe.coord=[0.0,0.0,0.0]
    pointe.coord[1]=position[1]
    pointe.coord[2]=position[2]
    pointe.coord[3]=position[3]
  
    pointe.dirCroiss=[0.0,0.0,0.0]
    pointe.dirCroiss[1]=direction[1]
    pointe.dirCroiss[2]=direction[2]
    pointe.dirCroiss[3]=direction[3]
  
    pointe.dirInit = [0.0,0.0,0.0]
    pointe.dirInit[1]=direction[1]
    pointe.dirInit[2]=direction[2]
    pointe.dirInit[3]=direction[3]
  
    return pointe
  end #= Fonction initialisePointe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function deflecMecaPointe(pointe, dirApresMeca, elong)
  
    teta=Float64(15.0) #= Angle autour de G, en degres =#
    vTire = MVector{3, Float32}([0.0,0.0,0.0])
    vTireN = MVector{3, Float32}([0.0,0.0,0.0])
    dirInt = MVector{3, Float32}([0.0,0.0,0.0])
  
    profondeur=pointe.coord[3]
    cont=iCMecaSol(sol,profondeur)
  
    if oCMecaSol(sol,profondeur)==1  #= Contrainte anisotrope verticale =#
      while true
        vTire[1]=(2.0*dRandUnif()-1.0)*sin(pi*teta/180.0)
        vTire[2]=(2.0*dRandUnif()-1.0)*sin(pi*teta/180.0)
        while true
          vTire[3]=dRandUnif()
          if vTire[3]>cos(pi*teta/180.0)
            break
          end
        end
        if vTireN[3]>cos(pi*teta/180.0)
          break
        end
      end
    
  
      dirInt[1]=pointe.dirCroiss[1]+(elong*vTireN[1]*cont)
      dirInt[2]=pointe.dirCroiss[2]+(elong*vTireN[2]*cont)
      dirInt[3]=pointe.dirCroiss[3]+(elong*vTireN[3]*cont)
  
    else 
      vTire[1]=2.0*dRandUnif()-1.0
      vTire[2]=2.0*dRandUnif()-1.0
      vTire[3]=2.0*dRandUnif()-1.0
      vTireN = norme(vTire,vTireN)
    end
  
    if prodScal(vTireN,pointe.dirCroiss)<0.0
      
      vTireN[1]=-vTireN[1]
      vTireN[2]=-vTireN[2]
      vTireN[3]=-vTireN[3]
    end
  
    dirInt[1]=pointe.dirCroiss[1]+(elong*vTireN[1]*cont)
    dirInt[2]=pointe.dirCroiss[2]+(elong*vTireN[2]*cont)
    dirInt[3]=pointe.dirCroiss[3]+(elong*vTireN[3]*cont)
    
    dirApresMeca = norme(dirInt,dirApresMeca)
  
    return dirApresMeca
  end #= Fonction deflecMecaPointe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function deflecGeoPointe(pointe, dirApresMeca, dirApresGeo, elong)
  #= Version avec plagiotropisme =#
    dirInt = MVector{3, Float32}([0.0,0.0,0.0])
    vGeoInt = MVector{3, Float32}([0.0,0.0,0.0])
    vGeo = MVector{3, Float32}([0.0,0.0,0.0])
  
    if P_tendanceDirTropisme == -1
      vGeo[1]=0.0                  #= Gravitropisme n?gatif =#
      vGeo[2]=0.0
      vGeo[3]=-1.0
      
    elseif P_tendanceDirTropisme == 0
      vGeoInt[1]=pointe.dirInit[1] #= Plagiotropisme =#
      vGeoInt[2]=pointe.dirInit[2]
      vGeoInt[3]=0.0
      vGeo = norme(vGeoInt,vGeo)
    elseif P_tendanceDirTropisme == 1
      vGeo[1]=0.0                  #= Gravitropisme positif =#
      vGeo[2]=0.0
      vGeo[3]=1.0
    elseif P_tendanceDirTropisme == 2
      vGeoInt[1]=pointe.dirInit[1] #= Exotropisme =#
      vGeoInt[2]=pointe.dirInit[2]
      vGeoInt[3]=pointe.dirInit[3]
      vGeo = norme(vGeoInt,vGeo)
    else
      vGeo[1]=0.0                  #= Gravitropisme positif =#
      vGeo[2]=0.0
      vGeo[3]=1.0
    end
  
    dirInt[1]=dirApresMeca[1]+(vGeo[1]*P_intensiteTropisme*elong*pointe.diametre)
    dirInt[2]=dirApresMeca[2]+(vGeo[2]*P_intensiteTropisme*elong*pointe.diametre)
    dirInt[3]=dirApresMeca[3]+(vGeo[3]*P_intensiteTropisme*elong*pointe.diametre)
  
    dirApresGeo = norme(dirInt,dirApresGeo)
    
    return dirApresGeo
  end #= Fonction deflecGeoPointe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function deflecSurfPointe(Pointe, dirApresGeo, dirApresSurf)
    profLim = Float64(50.0*dRandUnif())
    dirInt = MVector{3, Float32}([0.0,0.0,0.0])
    dirInt[1]=dirApresGeo[1]
    dirInt[2]=dirApresGeo[2]
    dirInt[3]=dirApresGeo[3]
  
    if dirInt[3]<0.0 && Pointe.coord[3]<profLim
      dirInt[3]=dirInt[3]/10.0
    end
  
    dirApresSurf = norme(dirInt,dirApresSurf)
    return dirApresSurf
  end #= Fonction deflecSurfPointe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function reorientePointe(pointe, elong)
    dirInt1 = MVector{3, Float32}([0.0,0.0,0.0])
    dirInt2 = MVector{3, Float32}([0.0,0.0,0.0])
    nouvDir = MVector{3, Float32}([0.0,0.0,0.0])
  
    dirInt1 = deflecMecaPointe(pointe, dirInt1, elong)
    dirInt2 = deflecGeoPointe(pointe, dirInt1, dirInt2, elong)
    nouvDir = deflecSurfPointe(pointe,dirInt2,nouvDir)
  
    pointe.dirCroiss[1]=nouvDir[1]
    pointe.dirCroiss[2]=nouvDir[2]
    pointe.dirCroiss[3]=nouvDir[3]
  end #= Fonction reorientePointe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function calcElongationPointe(pointe, sol)
  #= Calcul de l'?longation potentielle affect?e par le sol =#
    if pointe.mature == 1 && pointe.stop == 0 && pointe.senile == 0 && pointe.diametre>P_diamMin
      return pointe.diametre*deltaT*P_penteVitDiam*croissSol(sol, pointe.coord[3])
    else
      return 0.0
    end
  end #= Fonction calcElongationPointe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function developpePointe(pointe)
    #= Assure l'?volution de la pointe, en la faisant vieillir et en changeant ses variables d'?tat au cours de sa vie =#
    pointe.age = pointe.age + deltaT#= Incr?mente l'?ge du m?rist?me selon le pas de temps =# 
  
    if pointe.mature==0 && pointe.age>P_ageMaturitePointe
      pointe.mature=1  #= Le primordium devient m?rist?me vrai =#
      pointe.age=0.0   #= Son ?ge est r?initialis? à 0 en tant que pointe mature =#
    end
    if pointe.mature==1 && pointe.stop==0 && pointe.age>(P_penteDureeCroissDiam2*pointe.diametre*pointe.diametre)
      pointe.stop=1 #= La pointe stoppe sa croissance =#
    end
    if pointe.mature==1 && pointe.stop==1 && pointe.senile==0 && pointe.age>(P_penteDureeVieDiamTMD*pointe.diametre*P_TMD)
      pointe.senile=1 #= La pointe devient s?nile =#
    end
  end #= Fonction developpePointe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function deplacePointe(pointe, elong)
   #= Assure le d?placement du m?rist?me suite à croissance axiale =#
  
    #= Sa position est modifi?e =#
    pointe.coord[1]=pointe.coord[1]+(elong*pointe.dirCroiss[1])
    pointe.coord[2]=pointe.coord[2]+(elong*pointe.dirCroiss[2])
    pointe.coord[3]=pointe.coord[3]+(elong*pointe.dirCroiss[3])
  
    #= Son attribut distPrimInit est modifi? =#
    pointe.distPrimInit+=elong
  
  end #= Fonction deplacePointe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function distInterRamifPointe(pointe, sol)
   #= Renvoie la valeur locale de la distance inter-ramification de la pointe =#
  
    return (P_distRamif*ramifSol(sol,pointe.coord[3]))
  
  end #= Fonction distInterRamifPointe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function detruitPointe(pointeADetruire)
  #= Supprime une pointe =#
    pointeADetruire = nothing
  end #= Fonction detruitPointe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function initialiseAxe(numAxe, diamPointe, origine, dirInit, axePere, segPorteur)
  #= Cette fonction retourne une nouvelle variable de type pTAxe,
    c'est-à-dire un pointeur sur le type Axe =#
    nouvAxe=Axe{Axe, Seg, Pointe}()
    premierSeg=initialiseSeg(sR.nbSegForm+1,origine,origine,diamPointe,nouvAxe,0,segPorteur)
    nouvAxe.pointe=initialisePointe(diamPointe,origine,dirInit)
    nouvAxe.premSeg=premierSeg
    nouvAxe.dernSeg=premierSeg
    nouvAxe.nbSeg=1
    nouvAxe.num=numAxe
    nouvAxe.pere=axePere
  
    nouvAxe.suivant=nullAxe
    nouvAxe.precedent=nullAxe
  
    return nouvAxe
  end #= Fonction initialiseAxe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function ajouteSegProlongeAxe(axe, segAAjouter)
  #= Cette fonction ajoute un segment de prolongement en position apicale
  à l'axe concern?, et incr?mente son compteur de segments =#
  
    ancienSegTerm=axe.dernSeg
  
    # Si ce dernier segment est complet, il faut prolonger la liste
    if ancienSegTerm.complet == 1
      ancienSegTerm.suiv=segAAjouter
      segAAjouter.prec=ancienSegTerm
      axe.dernSeg=segAAjouter
      axe.nbSeg+=1
    
    # Sinon, il faut juste compl?ter le dernier segment
    else 
      # rien à faire
      # les mises à jour sont faites dans developpeAxeSR
      # on ne doit pas passer ici
    end
  
  end #= Fonction ajouteSegProlongeAxe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function ajouteAxeSR(sR, axeAAjouter)
  #= Cette fonction ins?re un axe dans la chaîne des axes du syst?me racinaire,
  elle incr?mente en même temps le compteur d'axes et de segments =#
  
    if sR.premAxe == nullAxe  #= Le syst?me racinaire est vide =#
      axeAAjouter.suivant=nullAxe
      axeAAjouter.precedent=nullAxe
      sR.premAxe=axeAAjouter
      sR.dernAxe=axeAAjouter
    
    else #= Le syst?me contient d?jà des axes, assure le chaînage double des axes =#
      axeAAjouter.suivant=nullAxe
      axeAAjouter.precedent=sR.dernAxe
      sR.dernAxe.suivant=axeAAjouter
      sR.dernAxe=axeAAjouter
    end
  
    sR.nbAxeForm+=1
    sR.nbSegForm+=1  # à chaque axe, un segment
  end #= Fonction ajouteAxeSR =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function latEmerge(diamAxePere)
     #= Renvoie 1 ou 0 suivant que la racine lat?rale ?merge ou pas =#
      #= Cela d?pend du diam?tre de l'axe p?re, et des deux param?tres P_probEmergeDmin et P_probEmergeDmax =#
      
    #= On commence par calculer la probabilit? d'?mergence pour un axe qui a pour diam?tre diamAxePere =#
    probaEmerge = P_probEmergeDmin + ((diamAxePere-P_diamMin)*(P_probEmergeDmax-P_probEmergeDmin)/(P_diamMax-P_diamMin))
    
    #= On compare un tirage al?atoire uniforme entre 0 et 1 ? la valeur de cette probabilit? et on retourne la comparaison =#
    return(probaEmerge > dRandUnif())
  end
   #= Fonction latEmerge =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function axeRamifiable(axe)
     #= Renvoie 1 ou 0 suivant que l'axe est ramifiable ou non =#
      # 3 conditions doivent ?tre r?unies (diam?tre, position, ?ge)
    if axe.pointe.diametre > 1.1*P_diamMin && axe.pointe.distPrimInit > P_distRamif && ((P_penteDureeCroissDiam2*axe.pointe.diametre*axe.pointe.diametre) - axe.pointe.age) > P_ageMaturitePointe
      return true
    else
      return false
    end
  end #= Fonction axeRamifiable =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function tireDiamPointeFille(axePere)
     #= Tire le diam?tre d'un m?rist?me de ramification suivant celui du p?re
         pour la ramification s?quentielle =#
  
      moy=(axePere.pointe.diametre*P_propDiamRamif) + (P_diamMin*(1.0-P_propDiamRamif))
      et=moy*P_coeffVarDiamRamif
      diamPFille=10000.0  # initialisation à une forte valeur pour boucle de tirage
      count = 1
    while diamPFille>(0.95*axePere.pointe.diametre)
      count =+ 1
      if count >= CONVERROR
        print("---------- \n \n")
        print("Error, P_propDiamRamif is too large, diameter cannot converge after %3i tries \n", CONVERROR)
        print("-> Change the value of P_propDiamRamif in the input file  \n")
        print("---------- \n \n")
        break
      end
      diamPFille = tireGaussien(moy,et)
    end
    return diamPFille
  end #= Fonction tireDiamPointeFille =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function tireDiamSem()
     #= Tire le diam?tre d'une pointe de s?minale, avec une certaine variation =#
  # Pas utilis? dans toutes les versions
    dmin=Float64(0.90*P_diamMax)
    dmax=Float64(1.0*P_diamMax)
    return drandUnifEntre(dmin,dmax)
  end #= Fonction tireDiamSem =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function origineAdv(segPere, origineFils)
    #= Calcule la position du point d'origine d'une tardive sur le seg p?re =#
  
    rel=dRandUnif()  #= definira la position relative sur le segment =#
    origineFils[1]=(rel*segPere.posO[1]) + ((1.0-rel)*segPere.posE[1])
    origineFils[2]=(rel*segPere.posO[2]) + ((1.0-rel)*segPere.posE[2])
    origineFils[3]=(rel*segPere.posO[3]) + ((1.0-rel)*segPere.posE[3])
    return origineFils
  end #= Fonction origineAdv =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function origineRamif(axePere, origineFils)
      #= Calcule la position du point d'origine d'une ramification =#
    origineFils[1]=axePere.pointe.coord[1]-
                      (axePere.pointe.distPrimInit*axePere.pointe.dirCroiss[1])
    origineFils[2]=axePere.pointe.coord[2]-
                      (axePere.pointe.distPrimInit*axePere.pointe.dirCroiss[2])
    origineFils[3]=axePere.pointe.coord[3]-
                      (axePere.pointe.distPrimInit*axePere.pointe.dirCroiss[3])
    return origineFils
  end #= Fonction origineRamif =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function orienteRamif(axePere, dirFils)
    #= Calcule la direction d'un axe fils issu de ramification =#
    vAxeRot = MVector{3, Float32}([0.0,0.0,0.0])
    rotDirCroiss = MVector{3, Float32}([0.0,0.0,0.0])
  
    #= Calcul de la norme de la projection direction sur plan horizontal =#
    norVProjHor=sqrt((axePere.pointe.dirCroiss[1]*axePere.pointe.dirCroiss[1])+
                    (axePere.pointe.dirCroiss[2]*axePere.pointe.dirCroiss[2]))
    if norVProjHor<epsilon
  
      vAxeRot[1]=1.0 #= Vecteur initial vertical =#
      vAxeRot[2]=0.0
      vAxeRot[3]=0.0 #= Vecteur (1,0,0) choisi pour axe de rotation =#
  
    else
  
      vAxeRot[1]=axePere.pointe.dirCroiss[2]/norVProjHor
      vAxeRot[2]=-axePere.pointe.dirCroiss[1]/norVProjHor
      vAxeRot[3]=0.0
  
    end
  
    #= On fait tourner dirCroiss autour de vAxeRot d'un angle d'insertion =#
    angRot=P_angLat
    rotVect(angRot,vAxeRot,axePere.pointe.dirCroiss,rotDirCroiss)
  
    #= On fait tourner rotDirCroiss autour de dirCroiss d'un angle radial =#
    angRot=tireAngRad()
    rotVect(angRot,axePere.pointe.dirCroiss,rotDirCroiss,dirFils)
  
  end #= Fonction orienteRamif =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function ramifieAxe(axePere)
  
    origRamif = MVector{3, Float32}([0.0,0.0,0.0])
    dirRamif = MVector{3, Float32}([0.0,0.0,0.0])
  
    #= D?cr?mente la distance au dernier primordium initi? =#
    axePere.pointe.distPrimInit-=distInterRamifPointe(axePere.pointe,sol)
  
    #= Calcul des attributs d'une ramification =#
    diamRamif=tireDiamPointeFille(axePere)    #= Tire le diam?tre de sa pointe =#
    
    if (diamRamif > P_diamMin) && latEmerge(axePere.pointe.diametre)  #= Si la racine est assez grosse et si elle a la chance d'?merger (? modifier) =#
      origRamif = origineRamif(axePere,origRamif)         #= Calcule sa position =#
      orienteRamif(axePere,dirRamif)          #= Calcule sa direction =#
      nouvAxe=initialiseAxe(sR.nbAxeForm+1,diamRamif,origRamif,dirRamif,axePere,axePere.dernSeg)
      ajouteAxeSR(sR,nouvAxe)
    end
  end #= Fonction ramifieAxe =#
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function developpeAxe(axe,taux)
    #= Assure le d?veloppement de l'axe, avec diff?rentes composantes =#
  
  
    elongation=taux*calcElongationPointe(axe.pointe,sol)
  
    axe.pointe.longueur+=elongation
  
    while axe.pointe.longueur > longSegNorm # on fait un segment "normal"
  
      axe.pointe.dateDerniereCreation=temps
  
      axe.pointe.longueur-=longSegNorm
  
      #= Calcule et affecte la nouvelle direction de croissance du m?rist?me =#
      reorientePointe(axe.pointe,longSegNorm)
  
      #= Le m?rist?me se d?place =#
      deplacePointe(axe.pointe,longSegNorm)
  
      if axe.dernSeg.complet == 1
        #= Il g?n?re un nouveau segment sur cet axe à sa nouvelle position =#
        sR.nbSegForm+=1
        nouvSeg=initialiseSeg(sR.nbSegForm,axe.dernSeg.posE,axe.pointe.coord,axe.pointe.diametre,axe,1,axe.dernSeg)
        ajouteSegProlongeAxe(axe,nouvSeg)
      
      else  # le premier segment est incomplet, on le modifie
        axe.dernSeg.complet=1
        axe.dernSeg.posE[1]=axe.pointe.coord[1]
        axe.dernSeg.posE[2]=axe.pointe.coord[2]
        axe.dernSeg.posE[3]=axe.pointe.coord[3]
        axe.dernSeg.jourForm=temps
      end
      
  #    printf(" Dans developpeAxe\n")
      while axeRamifiable(axe) 
        ramifieAxe(axe) # on ramifie ?ventuellement
      end
    end # fin du while  (axe.pointe.longueur > longSegNorm)
  
    if (temps - axe.pointe.dateDerniereCreation) > dureeSansCreation && axe.pointe.longueur > longSegMin #= production segment court  =#
  
      axe.pointe.dateDerniereCreation=temps
  
  
      #= Calcule et affecte la nouvelle direction de croissance de la pointe =#
      reorientePointe(axe.pointe,axe.pointe.longueur)
  
      #= La pointe se d?place =#
      deplacePointe(axe.pointe,axe.pointe.longueur)
  
      #= Elle g?n?re un nouveau segment sur cet axe à sa nouvelle position =#
      if axe.dernSeg.complet == 1
        #= Il g?n?re un nouveau segment sur cet axe à sa nouvelle position =#
        sR.nbSegForm+=1
        nouvSeg=initialiseSeg(sR.nbSegForm,axe.dernSeg.posE,axe.pointe.coord,axe.pointe.diametre,axe,1,axe.dernSeg)
        ajouteSegProlongeAxe(axe,nouvSeg)
      
      else  # le premier segment est incomplet, on le modifie
        axe.dernSeg.complet=1
        axe.dernSeg.posE[1]=axe.pointe.coord[1]
        axe.dernSeg.posE[2]=axe.pointe.coord[2]
        axe.dernSeg.posE[3]=axe.pointe.coord[3]
        axe.dernSeg.jourForm=temps
      end
  
      axe.pointe.longueur=0.0 # remet la longueur en attente du m?rist?me à 0
  
      while axeRamifiable(axe) 
        ramifieAxe(axe)
      end # on ramifie ?ventuellement
  
    end # fin du if (production d'un segment court)
  
  end #= Fonction developpeAxe =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function calcTSatisMoySR(sR)
  
  #= Calcul du taux de satisfaction moyen sur la p?riode ?coul?e (de 1 à temps) =#
  
    tSatisCum=Float64(0.0)
  
    for date in 2:temps+1 #= Boucle sur la p?riode ?coul?e =#
      tSatisCum+=sR.tSatis[date]
    end
    sR.tSatisMoy=tSatisCum/temps
  
  end  #= Fonction calcTSatisMoySR =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function calcTauxSatis(sR)
  
    calcTSatisMoySR(sR)
    
    if sR.biomUtilisee[temps] < 0.0002 
      return 1.0   # en d?but de croissance notamment, on ne g?re pas la limitation ?ventuelle
  
    else 
        biomU0=0.0002
        if temps>1 
        biomU0=sR.biomUtilisee[temps-1] # biomasse utilis?e au pas de temps - 2, s'il existe
      end  
        biomU1=sR.biomUtilisee[temps]    # biomasse utilis?e au pas de temps - 1
        biomU2=biomU1*(1+((biomU1-biomU0)/(0.5*(biomU0+biomU1))))   # biomasse au pas de temps t (non connue, c'est juste une estimation) 
        
        biomD1=sR.biomMax[temps] # biomasse disponible au pas de temps -1
        biomD2=sR.biomMax[temps+1]   # biomasse disponible au pas de temps actuel
        
        taux1=biomD1/biomU1
      if taux1>1.0 
        taux1=1.0
      end
        
        taux2=biomD2/biomU2
      if taux2>1.0 
        taux2=1.0
      end
    end
   return taux1^4.0 
  #  if (taux1<taux2) { return taux1 }
  #	            else { return taux2 }  # on renvoie le plus petit des 2 taux calcul?s
  
  end #= Fonction calcTauxSatis =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function calcDemandeVolume(axe)
  
  #= Calcule la demande en volume correspondant à la croissance en longueur
     pour un axe donn? =#
  
    return pi*(axe.pointe.diametre)*(axe.pointe.diametre)*calcElongationPointe(axe.pointe,sol)/4.0
  
  end #= Fonction calcDemandeVolume =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function detruitAxe(axeADetruire)
  #= Supprime un axe en supprimant ses segments, puis l'axe lui-même =#
  
    #= Lib?rer tous les segments de cet axe =#
    segCour=axeADetruire.premSeg
    while segCour.suiv!=nullSeg
    
      segAEnlever=segCour
      segCour=segCour.suiv
  #    if (ndCour->suivSPere!=nothing) { printf("Probl?me : Axe ramifi? à enlever\n") exit(1) }
      detruitSeg(segAEnlever)
    end
  
    detruitSeg(segCour) #= Enl?ve le segment apical =#
  
    detruitPointe(axeADetruire.pointe)
  
    #= Enlever l'axe en m?moire =#
    axeADetruire = nothing
  
  end #= Fonction detruitAxe =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function axeToutNecrose(axe)
  #= Cette fonction retourne la valeur 1 si l'axe a tous ses segments n?cros?s et 0 sinon =#
  
    resu=1 # on initialise la valeur r?sultat à vrai (1)
  
    if axe.pointe.senile == 0 
      resu=0 # non tout n?cros? si pointe non s?nile
    end
    segCour=axe.premSeg
    while segCour != nullSeg
      if segCour.necrose==0 
        resu=0
      end  # non tout n?cros? si un segment non n?cros?
      segCour=segCour.suiv
    end
    return resu
  end #= Fonction axeToutNecrose =#
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function affecValNecroseAxe(axe, valNecrose)
  #= Cette fonction affecte à chacun des segments de l'axe
     la valeur de necrose (0 ou 1) =#
  
    segCour=axe.premSeg
    while segCour != nullSeg
      segCour.necrose=valNecrose
      segCour=segCour.suiv
      # fin du while
    end
  end #= Fonction affecValNecroseAxe =#
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function affecValNecroseAmont(axe, valNecrose)
  #= Cette fonction affecte a chacun des segments en amont de l'axe
  la valeur de necrose (0 ou 1) =#
  
    segCour=axe.dernSeg
    while segCour != nullSeg
      segCour.necrose=valNecrose
      segCour=segCour.prec
    end
  end #= Fonction affecValNecroseAmont =#
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function affecValDiamAxe(axe, diam)
  #= Cette fonction affecte à chacun des segments de l'axe
     la valeur de diam?tre diam =#
  
    segCour=axe.premSeg
    while segCour != nullSeg
      segCour.diametre=diam
      segCour=segCour.suiv
    end  # fin du while
  end #= Fonction affecValDiamAxe =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function increValDiamAmont(axe, diam, coeff)
  #= Cette fonction incremente le diametre de chacun des noeuds en amont de l'axe =#
  
    segCour=axe.premSeg.prec # segment duquel l'axe est segment lat?ral
    while segCour != nullSeg
      diamInit=segCour.diametre
      section=(pi*diamInit*diamInit/4.0)+(pi*coeff*diam*diam/4.0)
      segCour.diametre=sqrt(4.0*section/pi)
      segCour=segCour.prec
    end # fin du while
  
  end #= Fonction increValDiamAmont =#
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function enleveAxeSR(sR, axeAEnlever)
  #= Cette fonction enl?ve un axe dans la chaîne des axes du syst?me racinaire =#
  
    axeDestructible=UInt8(0)
  
    if sR.premAxe==nullAxe  #= Le syst?me racinaire est vide =#
    
      printf("ATTENTION, probleme dans enleveAxeSR, sR vide \n")
      return
    
    else
    
      if axeAEnlever.precedent != nullAxe && axeAEnlever.suivant != nullAxe
        # On pourra le supprimer, on refait le chaînage
        axeAEnlever.precedent.suivant=axeAEnlever.suivant
        axeAEnlever.suivant.precedent=axeAEnlever.precedent
        axeDestructible=1
       # fin du if !=nothing && !=nothing
  
      elseif axeAEnlever.precedent == nullAxe && axeAEnlever.suivant != nullAxe
        # On pourra le supprimer, on refait le chaînage
        axeAEnlever.suivant.precedent=nullAxe
        sR.premAxe=axeAEnlever.suivant
        axeDestructible=1
       # fin du if ==nothing && !=nothing
  
      elseif axeAEnlever.precedent != nullAxe && axeAEnlever.suivant == nullAxe
        # On pourra le supprimer, on refait le chaînage
        axeAEnlever.precedent.suivant=nullAxe
        sR.dernAxe=axeAEnlever.precedent
        axeDestructible=1
       # fin du if !=nothing && ==nothing
  
      elseif axeAEnlever.precedent == nullAxe && axeAEnlever.suivant == nullAxe
        # On ne pourra pas le supprimer, car il est seul
        axeDestructible=0
      end # fin du if ==nothing && ==nothing
  
      if axeDestructible == 1
        sR.nbAxeSup += 1
        detruitAxe(axeAEnlever) # D?truit ses segments, sa pointe, et lui-même
      end
    end
  end #= Fonction enleveAxeSR =#
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function initialiseSR(origine)
  
  #= Initialisation du syst?me racinaire =#
    sR=SysRac{Axe}()  #= Cr?ation d'un syst?me racinaire =#
  
    sR.nbAxeForm=0  #= Initialisation des variables =#
    sR.nbAxeSup=0
    sR.nbSegForm=0
    sR.nbSem=0  #= Nombre de racines s?minales ?mises =#
    sR.nbAdv=0  #= Nombre de racines adventives ?mises=#
    sR.longueur=0.0
    sR.premAxe=nullAxe
    sR.dernAxe=nullAxe
    sR.tSatisMoy=1
    sR.volTotPrec=0.0
    sR.volTot=0.0
    sR.origine = [0.0,0.0,0.0]
    sR.origine[1]=origine[1]  #= Coordonn?es de l'origine du syst?me racinaire =#
    sR.origine[2]=origine[2]
    sR.origine[3]=origine[3]
  
    sR.angDep=2.0*pi*dRandUnif()  #= Orientation =#
    sR.tSatis = repeat([0.0], NBPASMAX)
    for i in 1:NBPASMAX
      sR.tSatis[i] = 1.0
    end
  
    return sR
  end  #= Fonction initialiseSR =#
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function longSeg(seg)
  #= Calcule la longueur d'un segment =#
  
    return sqrt(((seg.posE[1]-seg.posO[1])*(seg.posE[1]-seg.posO[1]))+
                ((seg.posE[2]-seg.posO[2])*(seg.posE[2]-seg.posO[2]))+
                ((seg.posE[3]-seg.posO[3])*(seg.posE[3]-seg.posO[3])))
  
  end  #= Fonction longSeg =#
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function calcNouvNbSem()
  #= Calcul du nouveau nombre de racines s?minales =#
  
    nouvNbSem=Int32(P_vitEmissionSem*temps)
  
    if nouvNbSem>=P_nbMaxSem 
      nouvNbSem=P_nbMaxSem
    end
  
    return nouvNbSem
  
  end  #= Fonction calcNouvNbSem =#
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function calcNouvNbAdv()
  
  #= Calcul du nouveau nombre de racines adventives =#
  
    nouvNbAdv=Int32(P_vitEmissionAdv*(temps-P_ageEmissionAdv))
  
    if nouvNbAdv>P_nbMaxAdv 
      nouvNbAdv=P_nbMaxAdv
    end
  
    return nouvNbAdv
  end  #= Fonction calcNouvNbAdv =#
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function emissionSemSR(sR)
  
  #= Emission de nouveaux axes s?minaux sur le syst?me racinaire =#
  
    vInit = MVector{3, Float32}([0.0,0.0,0.0]) 
    dirInit = MVector{3, Float32}([0.0,0.0,0.0])
   
  #  float diamSem=0.0f
  
    nbSemAEmettre=calcNouvNbSem() - sR.nbSem #= Nombre de s?minales à ?mettre =#
    for numSem in 1:nbSemAEmettre
      if sR.nbSem==0 
        angI=tireGaussien(0.0,0.06) # ?mission de la radicule proche verticale (gravitropisme initial fort)
      else 
        angI=tireGaussien(P_angInitMoyVertSem,P_angInitETVertSem)
      end
      vInit[1]=sin(angI)
      vInit[2]=0.0
      vInit[3]=cos(angI)
      angRot=sR.angDep+tireAngRad()
      dirInit = rotZ(vInit,dirInit,angRot)
  
      #= G?n?ration de l'axe et int?gration dans le syst?me racinaire =#
  #    diamSem=tireDiamSem()    
      nouvAxe=initialiseAxe(sR.nbAxeForm+1,P_propDiamSem*P_diamMax,sR.origine,dirInit,nullAxe,nullSeg)
      ajouteAxeSR(sR,nouvAxe)
      sR.nbSem+=1
    end
end  #= Fonction emissionSemSR =#
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function emissionAdvSR(sR)
  
  #= Emission de nouveaux axes adventifs sur le syst?me racinaire, 
     elles apparaissent sur la radicule (premi?re s?minale) =#
  
    vInit = MVector{3, Float32}([0.0,0.0,0.0]) 
    dirInit = MVector{3, Float32}([0.0,0.0,0.0])
    posInit = MVector{3, Float32}([0.0,0.0,0.0])
    
    nbAdvAEmettre=calcNouvNbAdv() - sR.nbAdv #= Nombre de racines adventives à ?mettre =#
    
    for numAdv in 1:nbAdvAEmettre
      #    printf("Je suis dans emissionAdvSR %3i \n",sR->nbTard)
      #= Calcul de la position initiale de l'axe =#
        #= Tirage de la distance à la base de cette adventive =#
        dBaseAdv=dRandUnif()*P_dBaseMaxAdv
  
        #= D?termination du segment p?re, sur le premier axe =#
        segPere=sR.premAxe.premSeg
        dBaseCour=longSeg(segPere)
        while dBaseCour < dBaseAdv && segPere.suiv != nullSeg
            segPere=segPere.suiv
            dBaseCour+=longSeg(segPere)
      end
  
        #= Position sur ce segment =#
        posInit = origineAdv(segPere,posInit)
      #= Calcul de la direction initiale de l'axe =#
      angI=tireGaussien(P_angInitMoyVertAdv,P_angInitETVertAdv) # angle par rapport à la verticale
      vInit[1]=sin(angI)
      vInit[2]=0.0
      vInit[3]=cos(angI)
      angRot=tireAngRad()
      dirInit = rotZ(vInit,dirInit,angRot)
  
      #= G?n?ration de l'axe et int?gration dans le syst?me racinaire =#
      nouvAxe=initialiseAxe(sR.nbAxeForm+1,P_propDiamAdv*P_diamMax,posInit,dirInit,sR.premAxe,segPere)
      ajouteAxeSR(sR,nouvAxe)
      sR.nbAdv+=1
    end
  end  #= Fonction emissionAdvSR =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function calcVolProdSR(sR)
  
  #= Calcul du volume racinaire produit sur la p?riode ?coul?e =#
    sR.volProd=0.0
    for date in 2:temps+1 #= Boucle sur la p?riode ?coul?e =#
    
      #  sR.volProd+=sR.volDem[date]*sR.tSatis[date]     faux apr?s changement des calculs
    end
  
  end  #= Fonction calcVolProdSR =#
  
  
  
  #=**************************************************************************=#
  #=**************************************************************************=#
  function readParamXML()
    #= Function to read the xml input file for the model =#
  
    #print("Reading XML parameter file \n")
    doc = parse_file(filename)
    
    # Get the root
    xroot = root(doc)
    global P_duree = parse(Int32, content(find_element(xroot, "sim_length")))
    global P_vitEmissionSem = parse(Float32, content(find_element(xroot, "erSem")))
    global P_propDiamSem = parse(Float32, content(find_element(xroot, "dSem")))
    global P_nbMaxSem = parse(Int32, content(find_element(xroot, "maxSem")))
    global P_ageEmissionAdv = parse(Float32, content(find_element(xroot, "ageAdv")))
    global P_dBaseMaxAdv = parse(Float32, content(find_element(xroot, "distAdv")))
    global P_vitEmissionAdv = parse(Float32, content(find_element(xroot, "erAdv")))
    global P_propDiamAdv = parse(Float32, content(find_element(xroot, "dAdv")))
    global P_nbMaxAdv = parse(Int32, content(find_element(xroot, "maxAdv")))
    global P_diamMin = parse(Float32, content(find_element(xroot, "dmin")))
    global P_diamMax = parse(Float32, content(find_element(xroot, "dmax")))
    global P_penteVitDiam = parse(Float32, content(find_element(xroot, "EL")))
    global P_tendanceDirTropisme = parse(Int32, content(find_element(xroot, "TrT")))
    global P_intensiteTropisme = parse(Float32, content(find_element(xroot, "TrInt")))
    global P_ageMaturitePointe = parse(Float32, content(find_element(xroot, "PDT")))
    global P_distRamif = parse(Float32, content(find_element(xroot, "IPD")))
    global P_probEmergeDmax = parse(Float32, content(find_element(xroot, "pdmax")))
    global P_probEmergeDmin = parse(Float32, content(find_element(xroot, "pdmin")))
    global P_probDiamRamif = parse(Float32, content(find_element(xroot, "RDM")))
    global P_coeffVarDiamRamif = parse(Float32, content(find_element(xroot, "CVDD")))
    global P_TMD = parse(Float32, content(find_element(xroot, "TMD")))
    global P_penteDureeCroissDiam2 = parse(Float32, content(find_element(xroot, "GDs")))
    global P_penteDureeVieDiamTMD = parse(Float32, content(find_element(xroot, "LDC")))
    global P_coeffCroissRad = parse(Float32, content(find_element(xroot, "SGC")))
  
    global P_exportType = parse(Int32, content(find_element(xroot, "exportType")))
    global P_exportName = content(find_element(xroot, "exportName"))
    #print("Parameters imported \n")
  end #= Function readParamXML =#

#=**************************************************************************=#
#=**************************************************************************=#
function origineEmission(nouvAxe)
    nouvAxe.pointe.coord[1]=sR.origine[1]
    nouvAxe.pointe.coord[2]=sR.origine[2]
    nouvAxe.pointe.coord[3]=sR.origine[3]
  end #= Fonction origineEmission =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function orienteEmission(nouvAxe, num)
    vInit = MVector{3, Float32}([0.0,0.0,0.0]) 
  
    angI=tireGaussien(P_angInitMoyVertSem,P_angInitETVertSem)
    vInit[1]=sin(angI)
    vInit[2]=0.0
    vInit[3]=cos(angI)
  
    angRot=sR.angDep+(2*pi*num/P_nbMaxSem)
    nouvAxe.pointe.dirCroiss = rotZ(vInit,nouvAxe.pointe.dirCroiss,angRot)
  end #= Fonction orienteEmission =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function volPrimSeg(seg)
  #= Calcule le volume primaire du segment =#
  
    return 0.25*pi*seg.axe.pointe.diametre*seg.axe.pointe.diametre*
    sqrt(((seg.posE[1]-seg.posO[1])*(seg.posE[1]-seg.posO[1]))+
         ((seg.posE[2]-seg.posO[2])*(seg.posE[2]-seg.posO[2]))+
         ((seg.posE[3]-seg.posO[3])*(seg.posE[3]-seg.posO[3])))
  
  end  #= Fonction volPrimSeg =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function volTotalSeg(seg)
  #= Calcule le volume total du segment =#
  
    return 0.25*pi*seg.diametre*seg.diametre*sqrt(((seg.posE[1]-seg.posO[1])*(seg.posE[1]-seg.posO[1]))+
                                                    ((seg.posE[2]-seg.posO[2])*(seg.posE[2]-seg.posO[2]))+
                                                    ((seg.posE[3]-seg.posO[3])*(seg.posE[3]-seg.posO[3])))
  
  end  #= Fonction volTotalSeg =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function distHorSeg(seg)
  #= Calcule la distance horizontale d'un segment =#
  
    return sqrt(((seg.posE[1]+seg.posO[1])*(seg.posE[1]+seg.posO[1])/4)+
                ((seg.posE[2]+seg.posO[2])*(seg.posE[2]+seg.posO[2])/4))
  
  end  #= Fonction distHorSeg =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function calcLimitesSR(sR)
  
  #= Calcul des limites du syst?me racinaire et de quelques autres variables =#
  
    # Initialisation des variables
    sR.volPrim=0.0  # volume des structures primaires
    sR.secPointe=0.0  # section totale des pointes actives
    sR.volTot=0.0   # volume total
    sR.longueur=0.0  # longueur
    sR.diamMax=-1.0e10 # diam?tre maximal, du plus gros segment
    sR.distMax=-1.0e10  # extension maximale
    sR.profMax=-1.0e10  # profondeur maximale
  
    sR.xbinf=+1.0e10 
    sR.ybinf=+1.0e10 
    sR.zbinf=+1.0e10 # initialisation des valeurs
    sR.xbsup=-1.0e10 
    sR.ybsup=-1.0e10 
    sR.zbsup=-1.0e10
  
    distHorLong=0.0
    profLong=0.0
  
    axeCour=sR.premAxe
    while axeCour != nullAxe  # Calcul du volume "demand?"
      sR.volTot+=pi*axeCour.pointe.diametre*axeCour.pointe.diametre*axeCour.pointe.longueur/4
      sR.volPrim+=pi*axeCour.pointe.diametre*axeCour.pointe.diametre*axeCour.pointe.longueur/4
      if axeCour.pointe.mature == 1 && axeCour.pointe.senile == 0
        sR.secPointe+=pi*axeCour.pointe.diametre*axeCour.pointe.diametre/4
      end
      segCour=axeCour.premSeg
      while segCour != nullSeg # Tant que ce segment existe
        # Calculs sur le segment courant segCour
        if (segCour.posO[1] < sR.xbinf) 
          sR.xbinf=segCour.posO[1]
        end
        if (segCour.posE[1] < sR.xbinf)
          sR.xbinf=segCour.posE[1]
        end
        if (segCour.posO[1] > sR.xbsup) 
          sR.xbsup=segCour.posO[1]
        end
        if (segCour.posE[1] > sR.xbsup) 
          sR.xbsup=segCour.posE[1]
        end
  
        if (segCour.posO[2] < sR.ybinf) 
          sR.ybinf=segCour.posO[2]
        end
        if (segCour.posE[2] < sR.ybinf) 
          sR.ybinf=segCour.posE[2]
        end
        if (segCour.posO[2] > sR.ybsup) 
          sR.ybsup=segCour.posO[2]
        end
        if (segCour.posE[2] > sR.ybsup) 
          sR.ybsup=segCour.posE[2]
        end
  
        if (segCour.posO[3] < sR.zbinf) 
          sR.zbinf=segCour.posO[3]
        end
        if (segCour.posE[3] < sR.zbinf) 
          sR.zbinf=segCour.posE[3]
        end
        if (segCour.posO[3] > sR.zbsup) 
          sR.zbsup=segCour.posO[3]
        end
        if (segCour.posE[3] > sR.zbsup) 
          sR.zbsup=segCour.posE[3]
        end
  
        if (segCour.diametre > sR.diamMax) 
          sR.diamMax=segCour.diametre
        end
  
        distHor=distHorSeg(segCour)
        if (distHor > sR.distMax) 
          sR.distMax=distHor
        end
  
        if (segCour.posO[3]>segCour.posE[3]) 
          profS=segCour.posO[3] 
        else 
          profS=segCour.posE[3]
        end
  
        if (profS > sR.profMax) 
          sR.profMax=profS
        end
  
        sR.volTot+=volTotalSeg(segCour)
        sR.volPrim+=volPrimSeg(segCour)
        longS=longSeg(segCour)
        sR.longueur+=longS
        distHorLong+=distHor*longS
        profLong+=profS*longS
  
        segCour=segCour.suiv
      end  # fin du while segCour
      axeCour=axeCour.suivant
    end  # fin du while axeCour
  
    sR.xbinf=sR.xbinf-d2 
    sR.xbsup=sR.xbsup+d2
    sR.ybinf=sR.ybinf-d2 
    sR.ybsup=sR.ybsup+d2
    sR.zbinf=sR.zbinf-d2 
    sR.zbsup=sR.zbsup+d2
  
    # Calcul de la maille de sol, en fonction de l'amplitude à balayer
  
    amplMax=0.0
    if (sR.xbsup-sR.xbinf) > amplMax 
      amplMax=sR.xbsup-sR.xbinf
    end
    if (sR.ybsup-sR.ybinf) > amplMax 
      amplMax=sR.ybsup-sR.ybinf
    end
    if (sR.zbsup-sR.zbinf) > amplMax 
      amplMax=sR.zbsup-sR.zbinf
    end
  
    maille=amplMax/(NBCASEMAX-1)
    if maille<mailleMin 
      maille=mailleMin
    end
    volElemSol=maille*maille*maille
  
  #  maille=5.0 volElemSol=maille*maille*maille
  
    sR.distMoy=distHorLong/sR.longueur
    sR.profMoy=profLong/sR.longueur
  
  #  printf(" xbinf :%7.2f",sR->xbinf) printf(" xbsup :%7.2f\n",sR->xbsup)
  #  printf(" ybinf :%7.2f",sR->ybinf) printf(" ybsup :%7.2f\n",sR->ybsup)
  #  printf(" zbinf :%7.2f",sR->zbinf) printf(" zbsup :%7.2f\n",sR->zbsup)
  #  printf(" amplMax :%7.2f",amplMax) printf(" maille :%7.2f\n",maille)
  
  
  end   #= Fonction calcLimitesSR =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function calcVolumesBiomassesSR(sR)
  
  #= Calcul des diff?rents volumes pour le syst?me racinaire avant le d?velopement sur le pas de temps =#
  
  
    # Initialisation des variables
    sR.volTotPrec=sR.volTot  # volume qui avait ?t? calcul? au pas de temps pr?c?dent
    
    sR.volPrim=0.0  # volume des structures primaires
    sR.volTot=0.0   # volume total ? calculer sur l'ensemble des segments et des pointes de tous les axes
  
    axeCour=sR.premAxe
    
    while axeCour != nullAxe  # Sur l'ensemble des axes
    
      sR.volTot+=pi*axeCour.pointe.diametre*axeCour.pointe.diametre*axeCour.pointe.longueur/4  # volume de la pointe
      sR.volPrim+=pi*axeCour.pointe.diametre*axeCour.pointe.diametre*axeCour.pointe.longueur/4
  
      segCour=axeCour.premSeg
      while segCour != nullSeg  # Tant qu'il y a des segments sur l'axe
        # Cumul des volumes
        sR.volTot+=volTotalSeg(segCour)
        sR.volPrim+=volPrimSeg(segCour)
        segCour=segCour.suiv
      end  # fin du while segCour
      axeCour=axeCour.suivant
    end # fin du while axeCour
    sR.biomUtilisee = repeat([0.0], NBPASMAX)
    sR.biomUtilisee[temps]=P_TMD*(sR.volTot - sR.volTotPrec)/1000  # Volume calcul? comme la diff?rence entre volume courant et le volume au jour pr?c?dent (NB peut ?tre n?gatif)
    if sR.biomUtilisee[temps] < 0.0001  
      sR.biomUtilisee[temps]=0.0001
    end  # Correction ?ventuelle, pour ?tre s?r de ne pas avoir une masse n?gative ou trop faible
    
  end  #= Fonction calcVolumesBiomassesSR =#
  #=**************************************************************************=#
  #=***********************************************************************=#
  function translateSR(sR)
    #= Translate le syst?me racinaire de façon à ce que tout se passe en territoire
   positif et d?marre de 0=#
  
    axeCour=sR.premAxe
    while axeCour != nullAxe  # Calcul du volume "demand?"
    
      # Translation de la pointe de l'axe
      axeCour.pointe.coord[1] -= sR.xbinf
      axeCour.pointe.coord[2] -= sR.ybinf
      axeCour.pointe.coord[3] -= sR.zbinf
  
      segCour=axeCour.premSeg
      while segCour != nullSeg # Tant qu'il y a des segments sur l'axe
        # Translation du segment segCour
        segCour.posO[1] -= sR.xbinf
        segCour.posO[2] -= sR.ybinf
        segCour.posO[3] -= sR.zbinf
  
        segCour.posE[1] -= sR.xbinf
        segCour.posE[2] -= sR.ybinf
        segCour.posE[3] -= sR.zbinf
  
        segCour=segCour.suiv
      end
      axeCour=axeCour.suivant
    end
  
    sR.xbsup-=sR.xbinf 
    sR.ybsup-=sR.ybinf 
    sR.zbsup-=sR.zbinf
    sR.xbinf=0.0 
    sR.ybinf=0.0 
    sR.zbinf=0.0
  
  end #= Fonction translateSR =#
  #=***********************************************************************=#
  #=***********************************************************************=#
  function initialiseTabSol()
  #= Initialise le tableau sol. La valeur 6 signifie que le point est à
     distance sup?rieure à d1 et d2 et d3 et d4 et d5 =#
     for i in 1:NBCASEMAX+1
      for j in 1:NBCASEMAX+1
        for k in 1:NBCASEMAX+1
          vox[i][j][k]=6
        end
      end
    end
  end #= Fonction initialiseTabSol =#
  #=***********************************************************************=#
  #=***********************************************************************=#
  function rangCase(coord)
   # renvoie le rang de la case du tableau des voxels dans laquelle est cette coordonn?e
  
    rang=Int32(coord/maille)
    if rang<0 
      return 0 
    elseif rang>NBCASEMAX 
      return NBCASEMAX
    end
    return rang
  
  end #= Fonction rangCase =#
  #=***********************************************************************=#
  #=***********************************************************************=#
  function coordPointCase(rangCase)
   # renvoie les coordonn?es d'un point dans la case
  
    return (((rangCase+0.5)*maille)+(0.5*maille*dRandUnif()))
  
  end #= Fonction coordPointCase =#
  #=***********************************************************************=#
  #=***********************************************************************=#
  function coordCentreCase(rangCase)
   # renvoie les coordonn?es du centre de la case
  
    return ((rangCase+0.5)*maille)
  
  end #= Fonction coordCentreCase =#
  #=***********************************************************************=#
  #=***********************************************************************=#
  function calcDistancesSR(sR)
  #= Calcule les distances entre mailles du sol et segments racinaires
     et calcule les volumes colonis?s =#
  
  #  float dist1,dist2   # si besoin
  
    axeCour=sR.premAxe
    while axeCour != nullAxe # Tant qu'il y a des axes dans le syst?me racinaire
    
      segCour=axeCour.premSeg
      if segCour.complet == 1
        while segCour != nullSeg  # Tant qu'il y a des segments sur l'axe
        
          xp1=segCour.posO[1] 
          yp1=segCour.posO[2] 
          zp1=segCour.posO[3]
          xp2=segCour.posE[1]
          yp2=segCour.posE[2] 
          zp2=segCour.posE[3]
  
          # Calcul des limites du domaine à explorer pour ce segment
          if xp1<xp2 
            xMin=xp1 - d5 
            xMax=xp2 + d5
          else 
            xMin=xp2 - d5 
            xMax=xp1 + d5
          end
          if yp1<yp2 
            yMin=yp1 - d5 
            yMax=yp2 + d5
          else 
            yMin=yp2 - d5 
            yMax=yp1 + d5
          end
          if zp1<zp2 
            zMin=zp1 - d5 
            zMax=zp2 + d5
          else 
            zMin=zp2 - d5 
            zMax=zp1 + d5
          end
  
            # balayage de ce domaine pertinent et calcul des distances
          for caseX in rangCase(xMin):rangCase(xMax)
            for caseY in rangCase(yMin):rangCase(yMax)
              for caseZ in rangCase(zMin):rangCase(zMax)
  
                xS=coordCentreCase(caseX) 
                yS=coordCentreCase(caseY) 
                zS=coordCentreCase(caseZ)
  
                # On calcule le projet? du point sol sur la droite contenant p1 et p2
                dx=xp2-xp1 
                dy=yp2-yp1 
                dz=zp2-zp1
                dM=(dx*xS)+(dy*yS)+(dz*zS)
  
                xProj=((dM*dx)+(xp1*((dy*dy)+(dz*dz)))-(zp1*dx*dz)-(yp1*dx*dy))/((dx*dx)+(dy*dy)+(dz*dz))
  
                if (xProj<=xp1 && xProj>=xp2)|(xProj>=xp1 && xProj<=xp2)
                 # Le projet? est entre les deux points du segment
                  yProj=((dM*dy)+(yp1*((dx*dx)+(dz*dz)))-(xp1*dx*dy)-(zp1*dz*dy))/((dx*dx)+(dy*dy)+(dz*dz))
                  zProj=((dM*dz)+(zp1*((dx*dx)+(dy*dy)))-(yp1*dz*dy)-(xp1*dz*dx))/((dx*dx)+(dy*dy)+(dz*dz))
                  distCour=sqrt(((xProj-xS)*(xProj-xS))+((yProj-yS)*(yProj-yS))+((zProj-zS)*(zProj-zS)))
                  # fin du if
                else
                  # Le projet? est à l'ext?rieur du segment
                #=
                  dist1=sqrt(((xp1-xS)*(xp1-xS))+((yp1-yS)*(yp1-yS))+
                             ((zp1-zS)*(zp1-zS)))
                  dist2=sqrt(((xp2-xS)*(xp2-xS))+((yp2-yS)*(yp2-yS))+
                             ((zp2-zS)*(zp2-zS)))
                  if (dist1<dist2) { distCour=dist1 } else { distCour=dist2 }
                =#
                distCour=2000.0
                end  # fin du else
  
                if (distCour<=d5) && (vox[caseX][caseY][caseZ]>5) 
                  vox[caseX][caseY][caseZ]=5
                end
                if (distCour<=d4) && (vox[caseX][caseY][caseZ]>4) 
                  vox[caseX][caseY][caseZ]=4
                end
                if (distCour<=d3) && (vox[caseX][caseY][caseZ]>3) 
                  vox[caseX][caseY][caseZ]=3
                end
                if (distCour<=d2) && (vox[caseX][caseY][caseZ]>2) 
                  vox[caseX][caseY][caseZ]=2
                end
                if (distCour<=d1) && (vox[caseX][caseY][caseZ]>1) 
                  vox[caseX][caseY][caseZ]=1
                end
  
              end # for du for caseZ
            end # for du for caseY
          end # for du for caseX
          segCour=segCour.suiv
        end  # fin du while (segCour!=nothing)
      end # fin du if (segCour->complet)
      axeCour=axeCour.suivant
    end # fin du while (axeCour!=nothing)
  
    sR.volSolD1=0.0  # initialisation avant cumul
    sR.volSolD2=0.0  # initialisation avant cumul
    sR.volSolD3=0.0  # initialisation avant cumul
    sR.volSolD4=0.0  # initialisation avant cumul
    sR.volSolD5=0.0  # initialisation avant cumul
    
    for i in 1:NBCASEMAX+1
      for j in 1:NBCASEMAX+1
        for k in 1:NBCASEMAX+1
          if vox[i][j][k]==5
            sR.volSolD5+=volElemSol
          end # fin du if
          if vox[i][j][k]==4
            sR.volSolD5+=volElemSol 
            sR.volSolD4+=volElemSol
          end # fin du if
          if vox[i][j][k]==3
            sR.volSolD5+=volElemSol 
            sR.volSolD4+=volElemSol 
            sR.volSolD3+=volElemSol
          end # fin du if
          if vox[i][j][k]==2
            sR.volSolD5+=volElemSol 
            sR.volSolD4+=volElemSol 
            sR.volSolD3+=volElemSol 
            sR.volSolD2+=volElemSol
          end # fin du if
          if vox[i][j][k]==1
            sR.volSolD5+=volElemSol 
            sR.volSolD4+=volElemSol 
            sR.volSolD3+=volElemSol 
            sR.volSolD2+=volElemSol 
            sR.volSolD1+=volElemSol
          end # fin du if
        end  # fin du for sur k
      end  # fin du for sur j
    end  # fin du for sur i
  
  
  end  #= Fonction calcDistancesSR  =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function developpeSR(sR)
  
  #= D?veloppement : croissance et ramification de chaque axe du syst?me =#
  
    calcVolumesBiomassesSR(sR)  # On calcule les volumes et les biomasses du syst?me racinaire pour la demande et le taux de satisfaction
    
    sR.tSatis[temps+1]=calcTauxSatis(sR)  # On calcule le taux de satifaction en utilisant l'info sur la biomasse utilis?e au jour pr?c?dent
  
    axeCour=sR.premAxe
    while axeCour != nullAxe  # D?veloppement
    
      developpeAxe(axeCour,sR.tSatis[temps+1]) # d?veloppe l'axe (croissance, ramif)
      developpePointe(axeCour.pointe)    # modifie les attributs de la pointe
      axeCour=axeCour.suivant
    
    end
    # printf(" Volume demand? : %16.5f \n",volumeDem)
    # printf(" NbRac : %6i \n",sR->nbAxeForm)
  end  #= Fonction developpeSR =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function mortaliteSR(sR)
  
    # Premier passage : calcul de la s?nilit? et affectation n?crose sur l'ensemble des axes =#
    axeCour=sR.premAxe # Dans le sens de premiers vers les derniers
    while axeCour != nullAxe
    
      if axeCour.pointe.senile == 1
       #= L'axe est n?cros? =#
        affecValNecroseAxe(axeCour, 1)
      
      else
        #= L'axe n'est pas n?cros? =#
        #= Tous les noeuds en amont de la pointe ne sont pas necros?s non plus =#
        affecValNecroseAmont(axeCour, 0)
      end
  
      axeCour=axeCour.suivant
    end
  
    # Calcul de l'?lagage, enl?vement des axes tout n?cros?s
    axeCour=sR.dernAxe # Dans le sens de derniers vers les premiers
    while axeCour != nullAxe
    
      if axeToutNecrose(axeCour) == 1
      
        axeAEnlever=axeCour
        axeCour=axeCour.precedent
        if axeAEnlever.pere != nullAxe
          enleveAxeSR(sR,axeAEnlever)
        end
      
      else 
        axeCour=axeCour.precedent
      end
    end
  end  #= Fonction mortaliteSR =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function croissanceRadialeSR(sR, coeffCroiss)
  
    #= Premier passage, initialisation aux diametres primaires =#
    axeCour=sR.dernAxe
    while axeCour != nullAxe
    
      diam=axeCour.pointe.diametre
      affecValDiamAxe(axeCour, diam)
      axeCour=axeCour.precedent
    end
  
    #= Deuxi?me passage, avec incr?ment des diametres =#
    axeCour=sR.dernAxe
    while axeCour != nullAxe
    
      #= les noeuds en amont sont incr?ment?s si axe en croissance (pointe mature et non senile) =#
      if (axeCour.pointe.mature == 1) && (axeCour.pointe.senile == 0)
      
        diam=axeCour.pointe.diametre
        increValDiamAmont(axeCour, diam, coeffCroiss)
      end
      axeCour=axeCour.precedent
    end
  end  #= Fonction croissanceRadialeSR =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function imprimeSeg(seg,args...)
  #= Imprime un segment sur le fichier des segments =#
    args = args[1][1][1]
    if length(args) == 1
      @printf(FSeg,"%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%s\n",
        seg.axe.num,seg.jourForm,seg.diametre,
        seg.posO[1],seg.posO[2],seg.posO[3],seg.posE[1],seg.posE[2],seg.posE[3],countSR)
    elseif length(args) == 3
      @printf(FSeg,"%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%s\t%f\n",
        seg.axe.num,seg.jourForm,seg.diametre,
        seg.posO[1],seg.posO[2],seg.posO[3],seg.posE[1],seg.posE[2],seg.posE[3],countSR,args[2])
    elseif length(args) == 5
      @printf(FSeg,"%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%s\t%f\t%f\n",
        seg.axe.num,seg.jourForm,seg.diametre,
        seg.posO[1],seg.posO[2],seg.posO[3],seg.posE[1],seg.posE[2],seg.posE[3],countSR,args[2],args[4])
    elseif length(args) == 7
      @printf(FSeg,"%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%s\t%f\t%f\t%f\n",
        seg.axe.num,seg.jourForm,seg.diametre,
        seg.posO[1],seg.posO[2],seg.posO[3],seg.posE[1],seg.posE[2],seg.posE[3],countSR,args[2],args[4],args[6])
    elseif length(args) == 9
      @printf(FSeg,"%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%s\t%f\t%f\t%f\t%f\n",
        seg.axe.num,seg.jourForm,seg.diametre,
        seg.posO[1],seg.posO[2],seg.posO[3],seg.posE[1],seg.posE[2],seg.posE[3],countSR,args[2],args[4],args[6],args[8])
    elseif length(args) == 11
      @printf(FSeg,"%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%s\t%f\t%f\t%f\t%f\t%f\n",
        seg.axe.num,seg.jourForm,seg.diametre,
        seg.posO[1],seg.posO[2],seg.posO[3],seg.posE[1],seg.posE[2],seg.posE[3],countSR,args[2],args[4],args[6],args[8],args[10])
    elseif length(args) == 13
      @printf(FSeg,"%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%s\t%f\t%f\t%f\t%f\t%f\t%f\n",
        seg.axe.num,seg.jourForm,seg.diametre,
        seg.posO[1],seg.posO[2],seg.posO[3],seg.posE[1],seg.posE[2],seg.posE[3],countSR,args[2],args[4],args[6],args[8],args[10],args[12])
    end
  end  #= Fonction imprimeSeg =#

function imprimeSRSegmentsEntete(args...)
    #= Imprime l'entête du fichier contenant les noeuds du syst?me racinaire =#
 
  # fprintf(FSeg,"NumSeg Jour NumAxe Suiv Prec Diam     X1       Y1       Z1      X2       Y2       Z2\n")
  if length(args) == 1
    @printf(FSeg,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tcountSR\n")
  elseif length(args) == 3
    @printf(FSeg,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tcountSR\t%s\n",args[1])
  elseif length(args) == 5
    @printf(FSeg,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tcountSR\t%s\t%s\n",args[1],args[3])
  elseif length(args) == 7
    @printf(FSeg,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tcountSR\t%s\t%s\t%s\n",args[1],args[3],args[5])
  elseif length(args) == 9
    @printf(FSeg,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tcountSR\t%s\t%s\t%s\t%s\n",args[1],args[3],args[5],args[7])
  elseif length(args) == 11
    @printf(FSeg,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tcountSR\t%s\t%s\t%s\t%s\t%s\n",args[1],args[3],args[5],args[7],args[9])
  elseif length(args) == 13
    @printf(FSeg,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tcountSR\t%s\t%s\t%s\t%s\t%s\t%s\n",args[1],args[3],args[5],args[7],args[9],args[11])
  end 
end  #= Fonction imprimeSRSegmentsEntete =#
 #=**************************************************************************=#
 #=**************************************************************************=#
function imprimeAxeSegments(axe,args...)
    #= Imprime les segments de l'axe =#
   segCour=axe.premSeg
   while segCour!=nullSeg
     if segCour.complet == 1
       imprimeSeg(segCour,args)
       
       #global countSegImprim += 1
     end
     segCour=segCour.suiv
   end
end #= Fonction imprimeAxeSegments =#
 #=**************************************************************************=#
 #=**************************************************************************=#
 function imprimeSRSegments(sR,args...)
   #= Imprime l'ensemble des segments du syst?me racinaire =#

   imprimeSRSegmentsEntete(args)
  
  
 
   axeCour=sR.premAxe
   
  while axeCour!= nullAxe
     imprimeAxeSegments(axeCour,args)
     axeCour=axeCour.suivant
  end
   
 end  #= Fonction imprimeSRSegments =#
#=**************************************************************************=#
#=**************************************************************************=#
function imprimeSRSegmentsEnteteRSML()
  #= Imprime l'entête du fichier contenant les noeuds du système racinaire =#

# @printf(FSeg,"NumSeg Jour NumAxe Suiv Prec Diam     X1       Y1       Z1      X2       Y2       Z2\n")
 #@printf(FSeg,"NumAxe Jour Diam     X1       Y1       Z1      X2       Y2       Z2\n")
 @printf(FSeg,"<?xml version='1.0' encoding='UTF-8'?>\n")
 @printf(FSeg,"<?xml version='1.0' encoding='UTF-8'?>\n")
 @printf(FSeg,"<rsml xmlns:po='http:#www.plantontology.org/xml-dtd/po.dtd'>\n")
 @printf(FSeg,"  <metadata>\n")
 @printf(FSeg,"    <version>1</version>\n")
 @printf(FSeg,"    <unit>mm</unit>\n")
 @printf(FSeg,"    <last-modified>today</last-modified>\n")
 @printf(FSeg,"    <software>archisimple</software>\n")
 @printf(FSeg,"    <species>Arabidopsis thaliana</species>\n")
 @printf(FSeg,"    <medium>null</medium>\n")
 @printf(FSeg,"    <treatment>null</treatment>\n")
 @printf(FSeg,"    <age_of_plant>")
 @printf(FSeg,"%i",temps)
 @printf(FSeg,"</age_of_plant>\n")
 @printf(FSeg,"    <id_of_plant>")
 @printf(FSeg,"%i",countSR)
 @printf(FSeg,"<id_of_plant>\n")

# Print the parameters of the simulation
 @printf(FSeg,"    <parameters>\n")  

 @printf(FSeg,"      <parameter name='P_duree'>")
 @printf(FSeg,"%i",P_duree)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_vitEmissionSem'>")
 @printf(FSeg,"%f",P_vitEmissionSem)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_nbMaxSem'>")
 @printf(FSeg,"%i",P_nbMaxSem)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_propDiamSem'>")
 @printf(FSeg,"%f",P_propDiamSem)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_ageEmissionAdv'>")
 @printf(FSeg,"%f",P_ageEmissionAdv)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_dBaseMaxAdv'>")
 @printf(FSeg,"%f",P_dBaseMaxAdv)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_vitEmissionAdv'>")
 @printf(FSeg,"%f",P_vitEmissionAdv)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_propDiamAdv'>")
 @printf(FSeg,"%f",P_propDiamAdv)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_nbMaxAdv'>")
 @printf(FSeg,"%i",P_nbMaxAdv)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_diamMin'>")
 @printf(FSeg,"%f",P_diamMin)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_diamMax'>")
 @printf(FSeg,"%f",P_diamMax)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_penteVitDiam'>")
 @printf(FSeg,"%f",P_penteVitDiam)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_tendanceDirTropisme'>")
 @printf(FSeg,"%i",P_tendanceDirTropisme)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_intensiteTropisme'>")
 @printf(FSeg,"%f",P_intensiteTropisme)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_ageMaturitePointe'>")
 @printf(FSeg,"%f",P_ageMaturitePointe)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_distRamif'>")
 @printf(FSeg,"%f",P_distRamif)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_probEmergeDmax'>")
 @printf(FSeg,"%f",P_probEmergeDmax)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_probEmergeDmin'>")
 @printf(FSeg,"%f",P_probEmergeDmin)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_propDiamRamif'>")
 @printf(FSeg,"%f",P_propDiamRamif)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_coeffVarDiamRamif'>")
 @printf(FSeg,"%f",P_coeffVarDiamRamif)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_TMD'>")
 @printf(FSeg,"%f",P_TMD)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_penteDureeCroissDiam2'>")
 @printf(FSeg,"%f",P_penteDureeCroissDiam2)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_penteDureeVieDiamTMD'>")
 @printf(FSeg,"%f",P_penteDureeVieDiamTMD)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"      <parameter name='P_coeffCroissRad'>")
 @printf(FSeg,"%f",P_coeffCroissRad)
 @printf(FSeg,"</parameter>\n")

 @printf(FSeg,"    </parameters>\n")  
 @printf(FSeg,"    <user>globet</user>\n")
 @printf(FSeg,"    <file-key>myimage</file-key>\n")
 @printf(FSeg,"    <property-definitions>\n")
 @printf(FSeg,"      <property-definition>\n")
 @printf(FSeg,"        <label>diameter</label>\n")
 @printf(FSeg,"          <type>float</type>\n")
 @printf(FSeg,"          <unit>cm</unit>\n")
 @printf(FSeg,"      </property-definition>\n")
 @printf(FSeg,"      <property-definition>\n")
 @printf(FSeg,"        <label>age</label>\n")
 @printf(FSeg,"          <type>int</type>\n")
 @printf(FSeg,"          <unit>day</unit>\n")
 @printf(FSeg,"      </property-definition>\n")
 @printf(FSeg,"    </property-definitions>\n")
 @printf(FSeg,"  </metadata>\n")
 @printf(FSeg,"  <scene>\n")
 @printf(FSeg,"    <plant>  \n")

end  #= Fonction imprimeSRSegmentsEnteteXML =#
#=**************************************************************************=#
#=**************************************************************************=#
function imprimeSegRSML(seg, last)
  #= Imprime un segment sur le fichier des segments =#
    if !last
      @printf(FSeg,"              <point x='")
      @printf(FSeg,"%f",seg.posO[1])
      @printf(FSeg,"' z='-")
      @printf(FSeg,"%f",seg.posO[3])
      @printf(FSeg,"' y='")
      @printf(FSeg,"%f",seg.posO[2])
      @printf(FSeg,"'/>\n")
    end
    if last
      @printf(FSeg,"              <point x='")
      @printf(FSeg,"%f",seg.posE[1])
      @printf(FSeg,"' z='-")
      @printf(FSeg,"%f",seg.posE[3])
      @printf(FSeg,"' y='")
      @printf(FSeg,"%f",seg.posE[2])
      @printf(FSeg,"'/>\n")
    end
  end  #= Fonction imprimeSegRSML =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function printAgeRSML(seg)
  #= Imprime un segment sur le fichier des segments =#
    @printf(FSeg,"              <sample>")
    @printf(FSeg,"%i", seg.jourForm)
    @printf(FSeg,"</sample>\n")
  end #= Fonction printAgeRSLM =#
   #=**************************************************************************=#
  #=**************************************************************************=#
  function printDiamRSML(seg)
  #= Imprime un segment sur le fichier des segments =#
    @printf(FSeg,"              <sample>")
    @printf(FSeg,"%f", seg.diametre)
    @printf(FSeg,"</sample>\n")
  end#= Fonction printDiamRSML =#
  #=**************************************************************************=#
  #=**************************************************************************=#
  function imprimeAxeSegmentsRSML(axe)
    #= Imprime les segments de l'axe =#
    @printf(FSeg,"        <geometry>  \n")
    @printf(FSeg,"          <polyline>  \n")
    segCour=axe.premSeg
    while segCour!=nullSeg
      if segCour.complet == 1 
        imprimeSegRSML(segCour, false)
      end
      segCour=segCour.suiv
    end
    segCour=axe.dernSeg
    if segCour.complet == 1
      imprimeSegRSML(segCour, true)
    end
  
    @printf(FSeg,"          </polyline>  \n")
    @printf(FSeg,"        </geometry>  \n")
  
    # Print the diameters
    @printf(FSeg,"        <functions>  \n")
    @printf(FSeg,"          <function name='diameter' domain='polyline'>  \n")
    segCour=axe.premSeg
    while segCour!=nullSeg
      if segCour.complet == 1 
        printDiamRSML(segCour)
      end
      segCour=segCour.suiv
    end
    segCour=axe.dernSeg
    if segCour.complet == 1 
      printDiamRSML(segCour)
    end
    @printf(FSeg,"          </function>  \n")
  
    # Print the age
    @printf(FSeg,"          <function name='age' domain='polyline'>  \n")
    segCour=axe.premSeg
    while segCour!=nullSeg
      if segCour.complet == 1 
        printAgeRSML(segCour)
      end
      segCour=segCour.suiv
    end
    segCour=axe.dernSeg
    if segCour.complet == 1 
      printAgeRSML(segCour)
    end
    @printf(FSeg,"          </function>  \n")
    @printf(FSeg,"        </functions>  \n")
  end  #= Fonction imprimeAxeSegmentsRSML =#
#=**************************************************************************=#
#=**************************************************************************=#
function imprimeSRSegmentsRSML(sR)
    #= Imprime l'ensemble des segments du système racinaire =#
  imprimeSRSegmentsEnteteRSML()

  axeCour=sR.premAxe
  while (axeCour!=nullAxe)
  
    # Print the primary axes
    if axeCour.pere==nullAxe

      # Count the number of segments
      count = 0
      segCour=axeCour.premSeg
      while segCour!=nullSeg
        if segCour.complet == 1 
          count +=1
        end
        segCour=segCour.suiv
      end
      if count > 1
        @printf(FSeg,"      <root ID='")
        @printf(FSeg,"%li",axeCour.num)
        @printf(FSeg,"' label='root' po:accession='PO:0009005'>  \n")
        imprimeAxeSegmentsRSML(axeCour)

        # Print the secondary axes
        axeCour1=sR.premAxe
        while axeCour1!=nullAxe
          if axeCour1.pere===axeCour
            count = 0
            segCour=axeCour1.premSeg
            while segCour!=nullSeg
              if segCour.complet == 1 
                count += 1
              end
              segCour=segCour.suiv
            end
            if count > 1
              @printf(FSeg,"      <root ID='")
              @printf(FSeg,"%li",axeCour1.num)
              @printf(FSeg,"' label='root' po:accession='PO:0009005'>  \n")
              imprimeAxeSegmentsRSML(axeCour1)

                  # Print the tertiary axes
                  axeCour2=sR.premAxe
                  while axeCour2!=nullAxe
                    if axeCour2.pere===axeCour1
                      count2 = 0
                      segCour=axeCour2.premSeg
                      while segCour!=nullSeg
                        if segCour.complet == 1 
                          count2 += 1
                        end
                        segCour=segCour.suiv
                      end
                      if count2 > 1
                        @printf(FSeg,"      <root ID='")
                        @printf(FSeg,"%li",axeCour1.num)
                        @printf(FSeg,"' label='root' po:accession='PO:0009005'>  \n")
                        imprimeAxeSegmentsRSML(axeCour2)

                        # Print the 4 order  axes
                          axeCour3=sR.premAxe
                          while axeCour3!=nullAxe
                            if axeCour3.pere === axeCour2
                              count3 = 0
                              segCour=axeCour3.premSeg
                              while segCour!=nullSeg
                                if segCour.complet == 1 
                                  count3 += 1
                                end
                                segCour=segCour.suiv
                              end
                              if count3 > 1
                                @printf(FSeg,"      <root ID='")
                                @printf(FSeg,"%li",axeCour1.num)
                                @printf(FSeg,"' label='root' po:accession='PO:0009005'>  \n")
                                imprimeAxeSegmentsRSML(axeCour3)

                                  # Print the 5 order  axes
                                  axeCour4=sR.premAxe
                                  while axeCour4!=nullAxe
                                    if axeCour4.pere === axeCour3
                                      count4 = 0
                                      segCour=axeCour4.premSeg
                                      while segCour!=nullSeg
                                        if segCour.complet == 1
                                          count4 += 1
                                        end
                                        segCour=segCour.suiv
                                      end
                                      if count4 > 1
                                        @printf(FSeg,"      <root ID='")
                                        @printf(FSeg,"%li",axeCour1.num)
                                        @printf(FSeg,"' label='root' po:accession='PO:0009005'>  \n")
                                        imprimeAxeSegmentsRSML(axeCour4)
                                        @printf(FSeg,"      </root>  \n")
                                      end
                                    end
                                    axeCour4=axeCour4.suivant
                                  end
                                @printf(FSeg,"      </root>  \n")
                              end
                            end
                            axeCour3=axeCour3.suivant
                          end
                        @printf(FSeg,"      </root>  \n")
                      end
                    end
                    axeCour2=axeCour2.suivant
                  end
              @printf(FSeg,"      </root>  \n")
            end
          end
          axeCour1=axeCour1.suivant
        end
        @printf(FSeg,"      </root>  \n")
      end
    end
    axeCour=axeCour.suivant
  end
  @printf(FSeg,"    </plant>  \n")
  @printf(FSeg,"  </scene>\n")
  @printf(FSeg,"</rsml>\n")
end  #= Fonction imprimeSRSegmentsRSML =#

#=**************************************************************************=#
#=**************************************************************************=#
function calcResumeSR(sR)
   #= Calcule les diff?rentes variables r?sum?es et ?criture sur fichier =#
 
 #  calcTSatisMoySR(sR)
   calcLimitesSR(sR)
 #  calcVolProdSR(sR)
   translateSR(sR)
   initialiseTabSol()
   calcDistancesSR(sR)
   imprimeSRGlobal(sR)
 
end  #= Fonction calcResumeSR =#
 #=**************************************************************************=#
 #=**************************************************************************=#
function fermeFichiers()
 
   close(FSeg)
   #close(FPar)
   close(FSol)
   close(FBiom)
 #  fclose(FAudit)
 #  fclose(FSynth)
 #  fclose(FVox)
 end  #= Fonction fermeFichiers =#
 #=**************************************************************************=#
 
 function countSegments(sR)
   
   countSeg = 0
   axeCour=sR.premAxe
   while axeCour!=nullAxe
   
     # Count the number of segments
     segCour=axeCour.premSeg
     while segCour!=nullSeg
       if segCour.complet==1 
         countSeg += 1
       end
       segCour=segCour.suiv
     end
     axeCour=axeCour.suivant
   end
 return countSeg
end



nullSeg = Seg{Axe, Seg}() #Pour référence nulle quand il n'y a pas de Seg
nullAxe = Axe{Axe,Seg,Pointe}() #Idem Axe 