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

hasCompoundName(G,S,N) :- 	atom_concat(X, S, N),
    						atom_concat(G, '_', X),
    						genus(G), species(S), hasParent(S,G).

isaStrict(A, A) :- 	order(A);
    				family(A);
    				genus(A);
    				hasCompoundName(_,_,A).
isaStrict(A, B) :-  hasParent(A,B), \+(species(A));
					hasParent(A,X), isaStrict(X,B), \+(species(A)).
isaStrict(A, B) :-  hasCompoundName(_,S,A), hasParent(S,B);
    				hasCompoundName(G,_,A), isaStrict(G,B).

isa(A,B) :-	hasCommonName(N,A), hasCommonName(M,B),
    		isaStrict(N,M).
isa(A,B) :- hasCommonName(N,A), \+(hasCommonName(_,B)),
    		isaStrict(N,B).
isa(A,B) :- \+(hasCommonName(_,A)), hasCommonName(M,B),
    		isaStrict(A,M).
isa(A,B) :- \+(hasCommonName(_,A)), \+(hasCommonName(_,B)),
    		isaStrict(A,B).
    		

synonym(A,B) :- hasCommonName(B,A), hasCommonName(B,_), A\=B.
synonym(A,B) :- hasCommonName(A,_), hasCommonName(A,B), A\=B.
synonym(A,B) :- hasCommonName(X,A), hasCommonName(X,B), A\=B.

countSpecies(A, N) :- hasCompoundName(_,_,A), N=1.
countSpecies(A, N) :- findall(_,speciesWithAncestor(_,A),X), sort(X,L), length(L,N).
countSpecies(_, 0).

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
range(ixobrychus_exilis, canada).
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

%Habitat
habitat(pelecanus_erythrorhynchos, lakePond).
habitat(pelecanus_occidentalis, ocean).
habitat(botaurus_lentiginosus, marsh).
habitat(ixobrychus_exilis, marsh).
habitat(ardea_herodias, marsh).
habitat(ardea_alba, marsh).
habitat(egretta_thula, marsh).
habitat(egretta_caerulea, marsh).
habitat(egretta_tricolor, marsh).
habitat(egretta_rufescens, marsh).
habitat(bubulcus_ibis, marsh).
habitat(butorides_virescens, marsh).
habitat(nycticorax_nycticorax, marsh).
habitat(nyctanassa_violacea, marsh).
habitat(eudocimus_albus, marsh).
habitat(plegadis_falcinellus, marsh).
habitat(plegadis_chihi, marsh).
habitat(platalea_ajaja, marsh).
habitat(platalea, marsh).
habitat(plegadis, marsh).
habitat(eudocimus, marsh).
habitat(nyctanassa, marsh).
habitat(nycticorax, marsh).
habitat(bubulcus, marsh).
habitat(butorides, marsh).
habitat(egretta, marsh).
habitat(ardea, marsh).
habitat(ixobrychus, marsh).
habitat(pelecanus, ocean).
habitat(pelecanus, lakePond).
habitat(pelecanidae, ocean).
habitat(pelecanidae, lakePond).
habitat(ardeidae, marsh).
habitat(threskiornithdae, marsh).
habitat(pelecaniformes, ocean).
habitat(pelecaniformes, lakePond).
habitat(pelecaniformes, marsh).

%Food
food(pelecanus_erythrorhynchos, fish).
food(pelecanus_occidentalis, fish).
food(botaurus_lentiginosus, fish).
food(ixobrychus_exilis, fish).
food(ardea_herodias, fish).
food(ardea_alba, fish).
food(egretta_thula, fish).
food(egretta_caerulea, fish).
food(egretta_tricolor, fish).
food(egretta_rufescens, fish).
food(bubulcus_ibis, insects).
food(butorides_virescens, fish).
food(nycticorax_nycticorax, fish).
food(nyctanassa_violacea, insects).
food(eudocimus_albus, insects).
food(plegadis_falcinellus, insects).
food(plegadis_chihi, insects).
food(platalea_ajaja, fish).
food(platalea, fish).
food(plegadis, insects).
food(eudocimus, insects).
food(nyctanassa, insects).
food(nycticorax, fish).
food(butorides, fish).
food(bubulcus, insects).
food(egretta, fish).
food(ardea, fish).
food(ixobrychus, fish).
food(pelecanus, fish).
food(pelecanidae, fish).
food(ardeidae, fish).
food(ardeidae, insects).
food(threskiornithdae, fish).
food(threskiornithdae, insects).
food(pelecaniformes, insects).
food(pelecaniformes, fish).

%Nesting
nesting(pelecanus_erythrorhynchos, ground).
nesting(pelecanus_occidentalis, tree).
nesting(botaurus_lentiginosus, ground).
nesting(ixobrychus_exilis, ground).
nesting(ardea_herodias, tree).
nesting(ardea_alba, tree).
nesting(egretta_thula, tree).
nesting(egretta_caerulea, tree).
nesting(egretta_tricolor, tree).
nesting(egretta_rufescens, tree).
nesting(bubulcus_ibis, tree).
nesting(butorides_virescens, tree).
nesting(nycticorax_nycticorax, tree).
nesting(nyctanassa_violacea, tree).
nesting(eudocimus_albus, tree).
nesting(plegadis_falcinellus, ground).
nesting(plegadis_chihi, ground).
nesting(platalea_ajaja, tree).
nesting(pelecanus, ground).
nesting(pelecanus, tree).
nesting(botaurus, ground).
nesting(ixobrychus, ground).
nesting(ardea, tree).
nesting(egretta, tree).
nesting(bubulcus, tree).
nesting(butorides, tree).
nesting(nycticorax, tree).
nesting(nyctanassa, tree).
nesting(eudocimus, tree).
nesting(plegadis, ground).
nesting(platalea, tree).
nesting(pelecanidae, ground).
nesting(pelecanidae, tree).
nesting(ardeidae, ground).
nesting(ardeidae, tree).
nesting(threskiornithdae, tree).
nesting(threskiornithdae, ground).
nesting(pelecaniformes, ground).
nesting(pelecaniformes, tree).

%Behaviour
behavior(pelecanus_erythrorhynchos, surfaceDive).
behavior(pelecanus_occidentalis, aerialDive).
behavior(botaurus_lentiginosus, stalking).
behavior(ixobrychus_exilis, stalking).
behavior(ardea_herodias, stalking).
behavior(ardea_alba, stalking).
behavior(egretta_thula, stalking).
behavior(egretta_caerulea, stalking).
behavior(egretta_tricolor, stalking).
behavior(egretta_rufescens, stalking).
behavior(bubulcus_ibis, groundForager).
behavior(butorides_virescens, stalking).
behavior(nycticorax_nycticorax, stalking).
behavior(nyctanassa_violacea, stalking).
behavior(eudocimus_albus, probing).
behavior(plegadis_falcinellus, probing).
behavior(plegadis_chihi, probing).
behavior(platalea_ajaja, probing).
behavior(pelecanus, surfaceDive).
behavior(pelecanus, aerialDive).
behavior(pelecanidae, surfaceDive).
behavior(ardeidae, stalking).
behavior(ardeidae, groundForager).
behavior(botaurus, stalking).
behavior(ixobrychus, stalking).
behavior(ardea, stalking).
behavior(egretta, stalking).
behavior(bubulcus, groundForager).
behavior(butorides, stalking).
behavior(nycticorax, stalking).
behavior(nyctanassa, stalking).
behavior(threskiornithdae, probing).
behavior(eudocimus, probing).
behavior(plegadis, probing).
behavior(platalea, probing).
behavior(pelecaniformes, surfaceDive).
behavior(pelecaniformes, aerialDive).
behavior(pelecaniformes, probing).
behavior(pelecaniformes, stalking).
behavior(pelecaniformes, groundForager).

%Conservation
conservation(pelecanus_erythrorhynchos, lc).
conservation(pelecanus_occidentalis, lc).
conservation(botaurus_lentiginosus, lc).
conservation(ixobrychus_exilis, lc).
conservation(ardea_herodias, lc).
conservation(ardea_alba, lc).
conservation(egretta_thula, lc).
conservation(egretta_caerulea, lc).
conservation(egretta_tricolor, lc).
conservation(egretta_rufescens, nt).
conservation(bubulcus_ibis, lc).
conservation(butorides_virescens, lc).
conservation(nycticorax_nycticorax, lc).
conservation(nyctanassa_violacea, lc).
conservation(eudocimus_albus, lc).
conservation(plegadis_falcinellus, lc).
conservation(plegadis_chihi, lc).
conservation(platalea_ajaja, lc).
conservation(pelecanus, lc).
conservation(botaurus, lc).
conservation(ixobrychus, lc).
conservation(ardea, lc).
conservation(egretta, lc).
conservation(egretta, nt).
conservation(bubulcus, lc).
conservation(butorides, lc).
conservation(nycticorax, lc).
conservation(nyctanassa, lc).
conservation(eudocimus, lc).
conservation(plegadis, lc).
conservation(platalea, lc).
conservation(pelecanidae, lc).
conservation(ardeidae, lc).
conservation(ardeidae, nt).
conservation(threskiornithdae, lc).
conservation(pelecaniformes, nt).
conservation(pelecaniformes, lc).
