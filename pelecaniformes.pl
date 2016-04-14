order(pelecaniformes).

family(pelecanidae).
family(ardeidae).
family(threskiornithdae).

genus(pelecanus).
genus(botaurus).
genus(ixobrychus).
genus(ardea).
genus(egretta).
genus(bubulcus).
genus(butorides).
genus(nycticorax).
genus(nyctanassa).
genus(eudocimus).
genus(plegadis).
genus(platalea).

species(erythrorhynchos).
species(occidentalis).
species(lentiginosus).
species(exilis).
species(herodias).
species(alba).
species(thula).
species(caerulea).
species(tricolor).
species(rufescens).
species(ibis).
species(virescens).
species(nycticorax).
species(violacea).
species(albus).
species(falcinellus).
species(chihi).
species(ajaja).

%hasParent(species,genus)
hasParent(erythrorhynchos, pelecanus).
hasParent(occidentalis, pelecanus).
hasParent(lentiginosus, botaurus).
hasParent(exilis, ixobrychus).
hasParent(herodias, ardea).
hasParent(alba, ardea).
hasParent(thula, egretta).
hasParent(caerulea, egretta).
hasParent(tricolor, egretta).
hasParent(rufescens, egretta).
hasParent(ibis, bubulcus).
hasParent(virescens, butorides).
hasParent(nycticorax, nycticorax).
hasParent(violacea, nyctanassa).
hasParent(albus, eudocimus).
hasParent(falcinellus, plegadis).
hasParent(chihi, plegadis).
hasParent(ajaja, platalea).
%hasParent(genus, family) ----------------------------
hasParent(pelecanus, pelecanidae).
hasParent(botaurus, ardeidae).
hasParent(ixobrychus, ardeidae).
hasParent(ardea, ardeidae).
hasParent(egretta, ardeidae).
hasParent(bubulcus, ardeidae).
hasParent(butorides, ardeidae).
hasParent(nycticorax, ardeidae).
hasParent(nyctanassa, ardeidae).
hasParent(eudocimus, threskiornithdae).
hasParent(plegadis, threskiornithdae).
hasParent(platalea, threskiornithdae).
%hasParent(family, order) ----------------------------
hasParent(pelecanidae, pelecaniformes).
hasParent(ardeidae, pelecaniformes).
hasParent(threskiornithdae, pelecaniformes).

hasParent2(A,B) :- hasCompoundName(_,S,A), genus(B), hasParent(S,B).
hasParent2(A,B) :- genus(A), family(B), hasParent(A,B).
hasParent2(A,B) :- family(A), order(B), hasParent(A,B).

hasCommonName(pelecanus,pelican).
hasCommonName(pelecanus_erythrorhynchos,americanWhitePelican).
hasCommonName(pelecanus_occidentalis,brownPelican).	
hasCommonName(botaurus,bittern).
hasCommonName(botaurus_lentiginosus,americanBittern).
hasCommonName(ixobrychus,bittern).
hasCommonName(ixobrychus_exilis,leastBittern).
hasCommonName(ardea,heron).
hasCommonName(ardea_herodias,greatBlueHeron).
hasCommonName(ardea_alba,greatEgret).
hasCommonName(egretta,heron).
hasCommonName(egretta,egret).
hasCommonName(egretta_thula,snowyEgret).
hasCommonName(egretta_caerulea,littleBlueHeron).
hasCommonName(egretta_tricolor,tricoloredHeron).
hasCommonName(egretta_rufescens,reddishEgret).
hasCommonName(bubulcus,egret).
hasCommonName(bubulcus_ibis,cattleEgret).
hasCommonName(butorides,heron).
hasCommonName(butorides_virescens,greenHeron).
hasCommonName(nycticorax,nightHeron).
hasCommonName(nycticorax_nycticorax,blackCrownedNightHeron).
hasCommonName(nyctanassa,nightHeron).
hasCommonName(nyctanassa_violacea,yellowCrownedNightHeron).
hasCommonName(eudocimus,ibis).
hasCommonName(eudocimus_albus,whiteIbis).
hasCommonName(plegadis,ibis).
hasCommonName(plegadis_falcinellus,glossyIbis).
hasCommonName(plegadis_chihi,whiteFacedIbis).
hasCommonName(platalea,spoonbill).
hasCommonName(platalea_ajaja,roseateSpoonbill).

hasCommonName(G, S, C) :- 	hasParent(S,G),
    						species(S), genus(G),
                            atom_concat(G,'_',X), atom_concat(X,S,Y),
    						hasCommonName(Y,C).

hasSciName(C, N) :- hasCommonName(N, C).

hasCompoundName(G,S,N) :- 	genus(G), species(S), hasParent(S,G),
    						atom_concat(G, '_', X), atom_concat(X, S, N).

isaStrict(A, A) :- 	order(A);
    				family(A);
    				genus(A);
    				hasCompoundName(_,_,A).
isaStrict(A, B) :-  hasParent2(A,B);
					(hasParent2(A,X), isaStrict(X,B)).

isa(A,B) :-	(nonvar(A), nonvar(B)) -> hasCommonName(N,A), hasCommonName(M,B),
    		isaStrict(N,M).
isa(A,B) :- nonvar(A) -> hasCommonName(N,A), isaStrict(N,B).
isa(A,B) :- nonvar(B) -> hasCommonName(M,B), isaStrict(A,M).
isa(A,B) :- isaStrict(A,B).
    		

synonym(A,B) :- hasCommonName(B,A), hasCommonName(B,_), A\=B.
synonym(A,B) :- hasCommonName(A,_), hasCommonName(A,B), A\=B.
synonym(A,B) :- hasCommonName(X,A), hasCommonName(X,B), A\=B.

countSpecies(A, N) :- hasCompoundName(_,_,A), N=1.
countSpecies(A, N) :- \+(hasCompoundName(_,_,A)), findall(_,speciesWithAncestor(_,A),X), sort(X,L), length(L,N).
countSpecies(A, N) :- \+(order(A)), \+(family(A)), \+(genus(A)), \+(hasCompoundName(_,_,A)), N=0.

%helper predicate.
speciesWithAncestor(A, B) :- species(A), genus(B), hasParent(A,B).
speciesWithAncestor(A, B) :- species(A), family(B), hasParent(A,X), hasParent(X,B).
speciesWithAncestor(A, B) :- species(A), order(B), hasParent(A,X), hasParent(X,Y), hasParent(Y,B).

%RangesTo
rangesTo(A,P) :- atom(A) -> range(A,P).
rangesTo(A,P) :- var(A) -> hasCompoundName(_,_,A), range(A,P).

range(pelecanus_erythrorhynchos, canada).
range(pelecanus_erythrorhynchos, alberta).
range(botaurus_lentiginosus, canada).
range(botaurus_lentiginosus, alberta).
range(ardea_herodias, canada).
range(ardea_herodias, alberta).
range(ardea_alba, canada).
range(bubulcus_ibis, canada).
range(butorides_virescens, canada).
range(nycticorax_nycticorax, canada).
range(nycticorax_nycticorax, alberta).
range(pelecaniformes, canada).
range(pelecaniformes, alberta).
range(pelecanus, canada).
range(pelecanus, alberta).
range(pelecanidae, canada).
range(pelecanidae, alberta).
range(botaurus, canada).
range(botaurus, alberta).
range(ardeidae, canada).
range(ardeidae, alberta).
range(ixobrychus, canada).
range(ardea, canada).
range(ardea, alberta).
range(bubulcus, canada).
range(butorides, canada).
range(nycticorax, canada).
range(nycticorax, alberta).

%habitats
habitat(A,P) :- atom(A) -> habitats(A,P).
habitat(A,P) :- var(A) -> hasCompoundName(_,_,A), habitats(A,P).

habitats(pelecanus_erythrorhynchos, lakePond).
habitats(pelecanus_occidentalis, ocean).
habitats(botaurus_lentiginosus, marsh).
habitats(ixobrychus_exilis, marsh).
habitats(ardea_herodias, marsh).
habitats(ardea_alba, marsh).
habitats(egretta_thula, marsh).
habitats(egretta_caerulea, marsh).
habitats(egretta_tricolor, marsh).
habitats(egretta_rufescens, marsh).
habitats(bubulcus_ibis, marsh).
habitats(butorides_virescens, marsh).
habitats(nycticorax_nycticorax, marsh).
habitats(nyctanassa_violacea, marsh).
habitats(eudocimus_albus, marsh).
habitats(plegadis_falcinellus, marsh).
habitats(plegadis_chihi, marsh).
habitats(platalea_ajaja, marsh).
habitats(platalea, marsh).
habitats(plegadis, marsh).
habitats(eudocimus, marsh).
habitats(nyctanassa, marsh).
habitats(nycticorax, marsh).
habitats(bubulcus, marsh).
habitats(butorides, marsh).
habitats(egretta, marsh).
habitats(ardea, marsh).
habitats(ixobrychus, marsh).
habitats(pelecanus, ocean).
habitats(pelecanus, lakePond).
habitats(pelecanidae, ocean).
habitats(pelecanidae, lakePond).
habitats(ardeidae, marsh).
habitats(threskiornithdae, marsh).
habitats(pelecaniformes, ocean).
habitats(pelecaniformes, lakePond).
habitats(pelecaniformes, marsh).

%diet
food(A,P) :- atom(A) -> diet(A,P).
food(A,P) :- var(A) -> hasCompoundName(_,_,A), diet(A,P).

diet(pelecanus_erythrorhynchos, fish).
diet(pelecanus_occidentalis, fish).
diet(botaurus_lentiginosus, fish).
diet(ixobrychus_exilis, fish).
diet(ardea_herodias, fish).
diet(ardea_alba, fish).
diet(egretta_thula, fish).
diet(egretta_caerulea, fish).
diet(egretta_tricolor, fish).
diet(egretta_rufescens, fish).
diet(bubulcus_ibis, insects).
diet(butorides_virescens, fish).
diet(nycticorax_nycticorax, fish).
diet(nyctanassa_violacea, insects).
diet(eudocimus_albus, insects).
diet(plegadis_falcinellus, insects).
diet(plegadis_chihi, insects).
diet(platalea_ajaja, fish).
diet(platalea, fish).
diet(plegadis, insects).
diet(eudocimus, insects).
diet(nyctanassa, insects).
diet(nycticorax, fish).
diet(butorides, fish).
diet(bubulcus, insects).
diet(egretta, fish).
diet(ardea, fish).
diet(ixobrychus, fish).
diet(pelecanus, fish).
diet(pelecanidae, fish).
diet(ardeidae, fish).
diet(ardeidae, insects).
diet(threskiornithdae, fish).
diet(threskiornithdae, insects).
diet(pelecaniformes, insects).
diet(pelecaniformes, fish).

%nestingPlacePlace
nesting(A,P) :- atom(A) -> nestingPlace(A,P).
nesting(A,P) :- var(A) -> hasCompoundName(_,_,A), nestingPlace(A,P).

nestingPlace(pelecanus_erythrorhynchos, ground).
nestingPlace(pelecanus_occidentalis, tree).
nestingPlace(botaurus_lentiginosus, ground).
nestingPlace(ixobrychus_exilis, ground).
nestingPlace(ardea_herodias, tree).
nestingPlace(ardea_alba, tree).
nestingPlace(egretta_thula, tree).
nestingPlace(egretta_caerulea, tree).
nestingPlace(egretta_tricolor, tree).
nestingPlace(egretta_rufescens, tree).
nestingPlace(bubulcus_ibis, tree).
nestingPlace(butorides_virescens, tree).
nestingPlace(nycticorax_nycticorax, tree).
nestingPlace(nyctanassa_violacea, tree).
nestingPlace(eudocimus_albus, tree).
nestingPlace(plegadis_falcinellus, ground).
nestingPlace(plegadis_chihi, ground).
nestingPlace(platalea_ajaja, tree).
nestingPlace(pelecanus, ground).
nestingPlace(pelecanus, tree).
nestingPlace(botaurus, ground).
nestingPlace(ixobrychus, ground).
nestingPlace(ardea, tree).
nestingPlace(egretta, tree).
nestingPlace(bubulcus, tree).
nestingPlace(butorides, tree).
nestingPlace(nycticorax, tree).
nestingPlace(nyctanassa, tree).
nestingPlace(eudocimus, tree).
nestingPlace(plegadis, ground).
nestingPlace(platalea, tree).
nestingPlace(pelecanidae, ground).
nestingPlace(pelecanidae, tree).
nestingPlace(ardeidae, ground).
nestingPlace(ardeidae, tree).
nestingPlace(threskiornithdae, tree).
nestingPlace(threskiornithdae, ground).
nestingPlace(pelecaniformes, ground).
nestingPlace(pelecaniformes, tree).

%Behaviour
behavior(A,P) :- atom(A) -> behaviors(A,P).
behavior(A,P) :- var(A) -> hasCompoundName(_,_,A), behaviors(A,P).

behaviors(pelecanus_erythrorhynchos, surfaceDive).
behaviors(pelecanus_occidentalis, aerialDive).
behaviors(botaurus_lentiginosus, stalking).
behaviors(ixobrychus_exilis, stalking).
behaviors(ardea_herodias, stalking).
behaviors(ardea_alba, stalking).
behaviors(egretta_thula, stalking).
behaviors(egretta_caerulea, stalking).
behaviors(egretta_tricolor, stalking).
behaviors(egretta_rufescens, stalking).
behaviors(bubulcus_ibis, groundForager).
behaviors(butorides_virescens, stalking).
behaviors(nycticorax_nycticorax, stalking).
behaviors(nyctanassa_violacea, stalking).
behaviors(eudocimus_albus, probing).
behaviors(plegadis_falcinellus, probing).
behaviors(plegadis_chihi, probing).
behaviors(platalea_ajaja, probing).
behaviors(pelecanus, surfaceDive).
behaviors(pelecanus, aerialDive).
behaviors(pelecanidae, surfaceDive).
behaviors(ardeidae, stalking).
behaviors(ardeidae, groundForager).
behaviors(botaurus, stalking).
behaviors(ixobrychus, stalking).
behaviors(ardea, stalking).
behaviors(egretta, stalking).
behaviors(bubulcus, groundForager).
behaviors(butorides, stalking).
behaviors(nycticorax, stalking).
behaviors(nyctanassa, stalking).
behaviors(threskiornithdae, probing).
behaviors(eudocimus, probing).
behaviors(plegadis, probing).
behaviors(platalea, probing).
behaviors(pelecaniformes, surfaceDive).
behaviors(pelecaniformes, aerialDive).
behaviors(pelecaniformes, probing).
behaviors(pelecaniformes, stalking).
behaviors(pelecaniformes, groundForager).

%conservation
conservation(A,P) :- atom(A) -> conservationStatus(A,P).
conservation(A,P) :- var(A) -> hasCompoundName(_,_,A), conservationStatus(A,P).
conservationStatus(pelecanus_erythrorhynchos, lc).
conservationStatus(pelecanus_occidentalis, lc).
conservationStatus(botaurus_lentiginosus, lc).
conservationStatus(ixobrychus_exilis, lc).
conservationStatus(ardea_herodias, lc).
conservationStatus(ardea_alba, lc).
conservationStatus(egretta_thula, lc).
conservationStatus(egretta_caerulea, lc).
conservationStatus(egretta_tricolor, lc).
conservationStatus(egretta_rufescens, nt).
conservationStatus(bubulcus_ibis, lc).
conservationStatus(butorides_virescens, lc).
conservationStatus(nycticorax_nycticorax, lc).
conservationStatus(nyctanassa_violacea, lc).
conservationStatus(eudocimus_albus, lc).
conservationStatus(plegadis_falcinellus, lc).
conservationStatus(plegadis_chihi, lc).
conservationStatus(platalea_ajaja, lc).
conservationStatus(pelecanus, lc).
conservationStatus(botaurus, lc).
conservationStatus(ixobrychus, lc).
conservationStatus(ardea, lc).
conservationStatus(egretta, lc).
conservationStatus(egretta, nt).
conservationStatus(bubulcus, lc).
conservationStatus(butorides, lc).
conservationStatus(nycticorax, lc).
conservationStatus(nyctanassa, lc).
conservationStatus(eudocimus, lc).
conservationStatus(plegadis, lc).
conservationStatus(platalea, lc).
conservationStatus(pelecanidae, lc).
conservationStatus(ardeidae, lc).
conservationStatus(ardeidae, nt).
conservationStatus(threskiornithdae, lc).
conservationStatus(pelecaniformes, nt).
conservationStatus(pelecaniformes, lc).
