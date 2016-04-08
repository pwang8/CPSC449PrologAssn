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

hasCommonName(pelecanus,pelican).
hasCommonName(erythrorhynchos,americanWhitePelican).
hasCommonName(occidentalis,brownPelican).	
hasCommonName(botaurus,bittern).
hasCommonName(lentiginosus,americanBittern).
hasCommonName(ixobrychus,bittern).
hasCommonName(exilis,leastBittern).
hasCommonName(ardea,heron).
hasCommonName(herodias,greatBlueHeron).
hasCommonName(alba,greatEgret).
hasCommonName(egretta,heron).
hasCommonName(egretta,egret).
hasCommonName(thula,snowyEgret).
hasCommonName(caerulea,littleBlueHeron).
hasCommonName(tricolor,tricoloredHeron).
hasCommonName(rufescens,reddishEgret).
hasCommonName(bubulcus,egret).
hasCommonName(ibis,cattleEgret).
hasCommonName(butorides,heron).
hasCommonName(virescens,greenHeron).
hasCommonName(nycticorax,nightHeron).
hasCommonName(nycticorax,blackCrownedNightHeron).
hasCommonName(nyctanassa,nightHeron).
hasCommonName(violacea,yellowCrownedNightHeron).
hasCommonName(eudocimus,ibis).
hasCommonName(albus,whiteIbis).
hasCommonName(plegadis,ibis).
hasCommonName(falcinellus,glossyIbis).
hasCommonName(chihi,whiteFacedIbis).
hasCommonName(platalea,spoonbill).
hasCommonName(ajaja,roseateSpoonBill).

hasCommonName(N,C) :- atom_concat(W, X, N),
                      atom_concat(Z, '_' , W),
                      hasCommonName(X, C), 
                      hasParent(X, Z).

hasCommonName(G, S, C) :- 	hasParent(S,G),
    						hasCommonName(S,C),
    						species(S), genus(G).

hasSciName(C, N) :- hasCommonName(N, C).

hasCompoundName(G,S,N) :- 	atom_concat(G, '_', X),
    						atom_concat(X, S, N),
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

    
    
    
    
