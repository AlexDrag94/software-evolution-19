module TraceabilityLinks

import analysis::stemming::Snowball;
import IO;
import lang::csv::IO;
import Relation;
import analysis::statistics::Frequency;
import Set;
import String;
import List;
import util::Math;

set[str] wordsInFile(loc file) 
  	= { word | /<word:[a-zA-Z]+>/ := readFile(file)};
  	
set[str] numInFile(loc file) 
  	= { word | /<word:[0-9]+>/ := readFile(file)};
  		
set[str] parsingAndTokenizing(set[str] x) {	
	return mapper(x, toLowerCase);
}

set[str] stopwordsRemoval(set[str] x, set[str] stopwords) {
	return x - stopwords;
}

list[str] stemAll(set[str] words) {
	list[str] req = [];
	for(x <- words) {
		req = req + stem(x);
	}
	return req;
}

list[str] prepReq(loc file) {
	stopwordsFile = |project://traceability-links/data/extra/stopwords.csv|; 
	x = wordsInFile(file);
	stopwords = wordsInFile(stopwordsFile);
	x = parsingAndTokenizing(x);
	x = stopwordsRemoval(x, stopwords);
	y = stemAll(x);
	return y;
}

list[num] createVector(list[str] x, list[str] master) {
	list[num] v = [];
	for(i <- master) {
		v = v + round((pct(x, i) * size(x)));
	}
	return v;	
}

list[str] createMaster(list[loc] paths) {
	list[str] master = [];		
	for( i <- paths) {
		master += prepReq(i);
	}
	return dup(master); 
}

rel[str req, list[num] vec] createVectorRelation
(list[str] master, list[loc] paths) {
	rel[str req, list[num] vec] reqs = {};
	for( i <- paths) {
		x = prepReq(i);
		reqs += <i.file, createVector(x, master)>;
	}
	return reqs;
}

list[num] createFrequencyMaster(set[list[num]] vecSet, int vecSize) {
	list[num] frequencyMaster = [];
		for(int i <- [0 .. vecSize]) {
		frequencyMaster += 0;
	}
	for(list[num] vec <- vecSet) {
		for(int i <- [0 .. vecSize]) {
			if(vec[i] > 0) {
				frequencyMaster[i] += 1;
			}
		}
	}
	return frequencyMaster;
}

rel[str req, list[num] vec] normalizeVectorRelation
(rel[str req, list[num] vec] reqs, list[num] frequencyMaster, num numReqs) {
	for(req <- reqs) {
		reqs -= req;
		for(i <- [0 .. size(frequencyMaster)]) {
			if(req.vec[i] > 0) {
				req.vec[i] *= log2(numReqs / frequencyMaster[i]);
			}
		}
		reqs += req;
	}
	return reqs;
}

rel[str high, str low, num sim] createSimilarityMatrix
(rel[str req, list[num] vec] highReqs, rel[str req, list[num] vec] lowReqs) {
	rel[str high, str low, num sim] similarityMatrix = {};
	for(highReq <- highReqs) {
		for(lowReq <- lowReqs) {
			sumXY = 0;
			sumX = 0;
			sumY = 0;
			sumFinal = 0;
			for(i <- [0 .. size(highReq.vec)]) {
				sumXY += highReq.vec[i] * lowReq.vec[i];
				sumX += highReq.vec[i] * highReq.vec[i];
				sumY += lowReq.vec[i] * lowReq.vec[i];			
			}
			sumFinal = sumXY / (sqrt(sumX) * sqrt(sumY));
			similarityMatrix += <highReq.req, lowReq.req, round(sumFinal, 0.001)>;			
		}
	}
	return similarityMatrix;
}

rel[str high, str low, num sim] createLinks
(rel[str high, str low, num sim] matrix, list[loc] high, list[loc] low, int mode) {
	rel[str high, str low, num sim] newMatrix = {};
	if (mode == 1) {
		newMatrix = matrix;
	}
	else if(mode == 2) {
		println(mode);
	}
	else if(mode == 3) {
		for(m <- matrix) {
			if(m.sim >= 0.25) {
				newMatrix += m;
			}
		}
	}
	else if(mode == 4) {
		for(x <- high) {
			for(y <- low) {
				if(sum(matrix[x.file, y.file]) >= 0.67 * max(matrix[x.file, _])) {
					newMatrix += <x.file, y.file, sum(matrix[x.file, y.file])>;
				}
			}
		}
	}
	else {
		for(x <- high) {
			for(y <- low) {
				if(sum(matrix[x.file, y.file]) >= 0.5 * max(matrix[x.file, _])) {
					newMatrix += <x.file, y.file, sum(matrix[x.file, y.file])>;
				}
			}
		}
	}
	return newMatrix;
}



void main() {
	high = |project://traceability-links/data/high|.ls; 
	low = |project://traceability-links/data/low|.ls;
	modeFile = |project://traceability-links/data/extra/input.csv|;
	modeSet = numInFile(modeFile);
	int mode = 1;
	for(m <- modeSet) {
		mode = toInt(m);
	}
	println(mode);

	list[str] master = [];
	
	rel[str req, list[num] vec] highReqs = {};
	rel[str req, list[num] vec] lowReqs = {};
	
	list[num] highFrequencyMaster = [];
	list[num] lowFrequencyMaster = [];
	list[num] frequencyMaster = [];
	
	rel[str high, str low, num sim] similarityMatrix = {};
	
	master = createMaster(high) + createMaster(low);
	
	highReqs = createVectorRelation(master, high);
	lowReqs = createVectorRelation(master, low);
	
	highFrequencyMaster = createFrequencyMaster(highReqs.vec, size(master));
	lowFrequencyMaster = createFrequencyMaster(lowReqs.vec, size(master));
	set[list[num]] highLowMasters = {highFrequencyMaster, lowFrequencyMaster};
	frequencyMaster = createFrequencyMaster(highLowMasters, size(master));
	
	numReqs = size(high) + size(low);
	highReqs = normalizeVectorRelation(highReqs, frequencyMaster, numReqs);
	lowReqs = normalizeVectorRelation(lowReqs, frequencyMaster, numReqs);

	similarityMatrix = createSimilarityMatrix(highReqs, lowReqs);
	
	similarityMatrix = createLinks(similarityMatrix, high, low, mode);
	
	for(h <- high) {
		println(h.file);
		for(l <- similarityMatrix[h.file]) {
			println(l);
		}
		println();
	}	
}
