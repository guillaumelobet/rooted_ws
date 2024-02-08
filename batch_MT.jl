using Dates
temps1 = Dates.now()
using Distributed


if nprocs() == 1
    print("Automated attribution of CPU's...\n")
    addprocs()
    print("Number of CPU attributed : ",nprocs()-1,"\n")
else
    print("Number of CPU specified : ",nprocs()-1,"\n")
end

print("Number of agruments : ",length(ARGS),"\n")
print("Number of variable parameters : ", trunc(Int,(length(ARGS)-1)/4), "\n")
@everywhere begin
cd(@__DIR__)
include("ArchiSimple_MT.jl")
#import .ArchiSimple


ArchiSimple.readParamXML()
using Printf

global stringBuffer

global tempBuffer

global mainCount = 0

global dictVariables = Dict()

global arguments = ARGS

id = Dict{Integer, String}(
    1 => "a",
    2 => "b",
    3 => "c",
    4 => "d",
    5 => "e",
    6 => "f",
    7 => "g",
    8 => "h",
    9 => "i",
    10 => "j",
    11 => "k",
    12 => "l",
    13 => "m",
    14 => "n",
    15 => "o",
    16 => "p",
    17 => "q",
    18 => "r",
    19 => "s",
    20 => "t",
    21 => "u",
    22 => "v",
    23 => "w",
    24 => "x",
    25 => "y",
    26 => "z"
)

end


if length(arguments) == 1
    print("Total number of root systems to generate : ", parse(Int,arguments[1]), "\n")

    @time @sync @distributed for i in 1:parse(Int,arguments[1])
        worker = id[workers()[1]]
        global mainCount+=1
        print("Root system # ", worker*string(mainCount), ".\n")
        ArchiSimple.main(worker*string(mainCount))
    end 
elseif length(arguments) == 5
    dictVariables = Dict(
        arguments[1] => (parse(Float64,arguments[2]):parse(Float64,arguments[3]):parse(Float64,arguments[4]))
        )

    print("Total number of root systems to generate : ", 
        trunc(Int,(((parse(Float64,arguments[4])-parse(Float64,arguments[2]))/parse(Float64,arguments[3]))+1)
        *parse(Float64,arguments[5])) , "\n")

    @sync @distributed for i in dictVariables[arguments[1]]
        for j in 1:parse(Int,arguments[5])
            worker = id[workers()[1]]
            global mainCount+=1
            print("Root system # ", worker*string(mainCount), ".\n")
            ArchiSimple.main(arguments[1],i, worker*string(mainCount))
        end
    end
elseif length(arguments) == 9
    global dictVariables = Dict(
        arguments[1] => (parse(Float64,arguments[2]):parse(Float64,arguments[3]):parse(Float64,arguments[4])), 
        arguments[5] => (parse(Float64,arguments[6]):parse(Float64,arguments[7]):parse(Float64,arguments[8]))
        )

    print("Total number of root systems to generate : ", 
        trunc(Int,(((parse(Float64,arguments[4])-parse(Float64,arguments[2]))/parse(Float64,arguments[3]))+1)
        *(((parse(Float64,arguments[8])-parse(Float64,arguments[6]))/parse(Float64,arguments[7]))+1)
        *parse(Float64,arguments[9])) , "\n")

    @time @sync @distributed for i in dictVariables[arguments[1]]
        for j in dictVariables[arguments[5]]
            for k in 1:parse(Int,arguments[9])
                worker = id[workers()[1]]
                global mainCount+=1
                print("Root system # ", worker*string(mainCount), ".\n")
                ArchiSimple.main(arguments[1],i,arguments[5],j,worker*string(mainCount))
            end
        end
    end
elseif length(arguments) == 13
    dictVariables = Dict(
        arguments[1] => (parse(Float64,arguments[2]):parse(Float64,arguments[3]):parse(Float64,arguments[4])), 
        arguments[5] => (parse(Float64,arguments[6]):parse(Float64,arguments[7]):parse(Float64,arguments[8])), 
        arguments[9] => (parse(Float64,arguments[10]):parse(Float64,arguments[11]):parse(Float64,arguments[12]))
        )

    print("Total number of root systems to generate : ", 
        trunc(Int,(((parse(Float64,arguments[4])-parse(Float64,arguments[2]))/parse(Float64,arguments[3]))+1)
        *(((parse(Float64,arguments[8])-parse(Float64,arguments[6]))/parse(Float64,arguments[7]))+1)
        *(((parse(Float64,arguments[12])-parse(Float64,arguments[10]))/parse(Float64,arguments[11]))+1)
        *parse(Float64,arguments[13])) , "\n")

    @time @sync @distributed for i in dictVariables[arguments[1]]
        for j in dictVariables[arguments[5]]
            for k in dictVariables[arguments[9]]
                for l in 1:parse(Int,arguments[13])
                    worker = id[workers()[1]]
                    global mainCount+=1
                    print("Root system # ", worker*string(mainCount), ".\n")
                    ArchiSimple.main(arguments[1],i,arguments[5],j,arguments[9],k,worker*string(mainCount))
                end
            end
        end
    end
elseif length(arguments) == 17
    dictVariables = Dict(
        arguments[1] => (parse(Float64,arguments[2]):parse(Float64,arguments[3]):parse(Float64,arguments[4])), 
        arguments[5] => (parse(Float64,arguments[6]):parse(Float64,arguments[7]):parse(Float64,arguments[8])), 
        arguments[9] => (parse(Float64,arguments[10]):parse(Float64,arguments[11]):parse(Float64,arguments[12])),
        arguments[13] => (parse(Float64,arguments[14]):parse(Float64,arguments[15]):parse(Float64,arguments[16]))
        )

    print("Total number of root systems to generate : ", 
        trunc(Int,(((parse(Float64,arguments[4])-parse(Float64,arguments[2]))/parse(Float64,arguments[3]))+1)
        *(((parse(Float64,arguments[8])-parse(Float64,arguments[6]))/parse(Float64,arguments[7]))+1)
        *(((parse(Float64,arguments[12])-parse(Float64,arguments[10]))/parse(Float64,arguments[11]))+1)
        *(((parse(Float64,arguments[16])-parse(Float64,arguments[14]))/parse(Float64,arguments[15]))+1)
        *parse(Float64,arguments[17])) , "\n")

    @sync @distributed for i in dictVariables[arguments[1]]
        for j in dictVariables[arguments[5]]
            for k in dictVariables[arguments[9]]
                for l in dictVariables[arguments[13]]
                    for m in 1:parse(Int,arguments[17])
                        worker = id[workers()[1]]
                        global mainCount+=1
                        print("Root system # ", worker*string(mainCount), ".\n")
                        ArchiSimple.main(arguments[1],i,arguments[5],j,arguments[9],k,arguments[13],l,worker*string(mainCount))
                    end
                end
            end
        end
    end
elseif length(arguments) == 21
    dictVariables = Dict(
        arguments[1] => (parse(Float64,arguments[2]):parse(Float64,arguments[3]):parse(Float64,arguments[4])), 
        arguments[5] => (parse(Float64,arguments[6]):parse(Float64,arguments[7]):parse(Float64,arguments[8])), 
        arguments[9] => (parse(Float64,arguments[10]):parse(Float64,arguments[11]):parse(Float64,arguments[12])),
        arguments[13] => (parse(Float64,arguments[14]):parse(Float64,arguments[15]):parse(Float64,arguments[16])),
        arguments[17] => (parse(Float64,arguments[18]):parse(Float64,arguments[19]):parse(Float64,arguments[20]))
        )
    print("Total number of root systems to generate : ", 
        trunc(Int,(((parse(Float64,arguments[4])-parse(Float64,arguments[2]))/parse(Float64,arguments[3]))+1)
        *(((parse(Float64,arguments[8])-parse(Float64,arguments[6]))/parse(Float64,arguments[7]))+1)
        *(((parse(Float64,arguments[12])-parse(Float64,arguments[10]))/parse(Float64,arguments[11]))+1)
        *(((parse(Float64,arguments[16])-parse(Float64,arguments[14]))/parse(Float64,arguments[15]))+1)
        *(((parse(Float64,arguments[20])-parse(Float64,arguments[18]))/parse(Float64,arguments[19]))+1)
        *parse(Float64,arguments[21])) , "\n")

    @sync @distributed for i in dictVariables[arguments[1]]
        for j in dictVariables[arguments[5]]
            for k in dictVariables[arguments[9]]
                for l in dictVariables[arguments[13]]
                    for m in dictVariables[arguments[17]]
                        for l in 1:parse(Int,arguments[21])
                            worker = id[workers()[1]]
                            global mainCount+=1
                            print("Root system # ", worker*string(mainCount), ".\n")
                            ArchiSimple.main(arguments[1],i,arguments[5],j,arguments[9],k,arguments[13],l,arguments[17],m,worker*string(mainCount))
                        end
                    end
                end
            end
        end
    end
else
    print("Nombre d'arguments incorrect, vérifiez les arguments et la syntaxe !")
end
#=
print("Écriture du fichier...\n")
ioFile = open("myroot.txt", "w")

if length(arguments) == 1
    @printf(ioFile,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tidSR\n")
  elseif length(arguments) == 5
    @printf(ioFile,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tidSR\t%s\n",arguments[1])
  elseif length(arguments) == 9
    @printf(ioFile,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tidSR\t%s\t%s\n",arguments[1],arguments[5])
  elseif length(arguments) == 13
    @printf(ioFile,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tidSR\t%s\t%s\t%s\n",arguments[1],arguments[5],arguments[9])
  elseif length(arguments) == 17
    @printf(ioFile,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tidSR\t%s\t%s\t%s\t%s\n",arguments[1],arguments[5],arguments[9],arguments[13])
  elseif length(arguments) == 21
    @printf(ioFile,"NumAxe\tJour\tDiam\tX1\tY1\tZ1\tX2\tY2\tZ2\tidSR\t%s\t%s\t%s\t%s\t%s\n",arguments[1],arguments[5],arguments[9],arguments[13],arguments[17])
end 

@printf(ioFile, "%s",stringBuffer)
close(ioFile)
print("Fichier sauvé !\n")
=#
temps2 = Dates.now()
print("Temps d'exécution total : ",temps2-temps1,"\n")

