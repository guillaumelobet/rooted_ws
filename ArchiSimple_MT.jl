module ArchiSimple

using Distributed
using StaticArrays
using Random
using Printf
using TypedTables
using Parsers
using Parameters
using LazilyInitializedFields
using LightXML
using Distributions
using Dates

include("params_MT.jl")    #= Importer le fichier contenant les définitions de paramètres et de constantes =#
include("fonctions_MT.jl") #= Importer le fichier contenant les fonctions =#



function main(args...)

    readParamXML()


    if length(args) == 1
        global countSR = args[1]
    elseif length(args) == 3
        @eval $(Symbol(args[1])) = $args[2]
        global countSR = args[3]
    elseif length(args) == 5
        @eval $(Symbol(args[1])) = $args[2]
        @eval $(Symbol(args[3])) = $args[4]
        global countSR = args[5]
    elseif length(args) == 7
        @eval $(Symbol(args[1])) = $args[2]
        @eval $(Symbol(args[3])) = $args[4]
        @eval $(Symbol(args[5])) = $args[6]
        global countSR = args[7]
    elseif length(args) == 9
        @eval $(Symbol(args[1])) = $args[2]
        @eval $(Symbol(args[3])) = $args[4]
        @eval $(Symbol(args[5])) = $args[6]
        @eval $(Symbol(args[7])) = $args[8]
        global countSR = args[9]
    elseif length(args) == 11
        @eval $(Symbol(args[1])) = $args[2]
        @eval $(Symbol(args[3])) = $args[4]
        @eval $(Symbol(args[5])) = $args[6]
        @eval $(Symbol(args[7])) = $args[8]
        @eval $(Symbol(args[9])) = $args[10]
        global countSR = args[11]
    elseif length(args) == 13
        @eval $(Symbol(args[1])) = $args[2]
        @eval $(Symbol(args[3])) = $args[4]
        @eval $(Symbol(args[5])) = $args[6]
        @eval $(Symbol(args[7])) = $args[8]
        @eval $(Symbol(args[9])) = $args[10]
        @eval $(Symbol(args[11])) = $args[12]
        global countSR = args[13]    
    end
    

    orig[1]=0.0 
    orig[2]=0.0 
    orig[3]=20.0

    global FSeg
    global FSol
    global FBiom

    global temps = 0



    FSeg, FSol, FBiom = ouvreFichiers()
    global sol = litSol()
    global sR = initialiseSR(orig)
    litBiomasseMaxSR(sR)
    global temps=0

    while temps<P_duree

        global temps+=deltaT
    #   printf("Temps : %3i \n",temps)

        #global segs = countSegments(sR)
        #print("Segments : ",segs, "\n")
        #segs = countSegments(sR)
        #= Emission des racines s?minales =#
        emissionSemSR(sR)

        #= Emission des racines adentives =#
        emissionAdvSR(sR)

        #= D?veloppement du syst?me racinaire =#
        developpeSR(sR)

        #= Croissance radiale du syst?me racinaire =#
        croissanceRadialeSR(sR, P_coeffCroissRad)

        #= Mortalit? du syst?me racinaire =#
        mortaliteSR(sR)

    end
    #=
    segs = countSegments(sR)
    print("NbSeg : ", segs, "\n")
    print("NbSegForm : ", sR.nbSegForm, "\n")
    print("NbAxeMort : ", sR.nbAxeSup, "\n")
    print("NbAxes : ", sR.nbAxeForm, "\n")
    =#
    #FSeg = IOBuffer()
    


    if P_exportType == 1
        imprimeSRSegments(sR,args)   # pour imprimer le syst?me racinaire sous forme d'un ensemble de segments 
    elseif P_exportType == 2
        imprimeSRSegmentsRSML(sR)
    end
    
    #print("CountSegImprim : ",countSegImprim, "\n")
    #return FSeg
    #  imprimeAudit()
    fermeFichiers()

    end

end