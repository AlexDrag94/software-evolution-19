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

list[str] wordsInFile(loc file) {
  	reqs = readFileLines(file);	
	list[str] reqs1 = [];
	for(r <- reqs) {
		if(r != "%") {
			reqs1 += split(" ", r);
		}		
	}
	return reqs1;
}

list[str] parsingAndTokenizing(list[str] x) {	
	return mapper(x, toLowerCase);
}

list[str] stopwordsRemoval(list[str] x, list[str] stopwords) {
	return x - stopwords;
}

list[str] stemAll(list[str] words) {
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
			sumXY = 0.0;
			sumX = 0.0;
			sumY = 0.0;
			sumFinal = 0.0;
			for(i <- [0 .. size(highReq.vec)]) {
				sumXY += (highReq.vec[i] * lowReq.vec[i]);
				sumX += (highReq.vec[i] * highReq.vec[i]);
				sumY += (lowReq.vec[i] * lowReq.vec[i]);			
			}
			sumFinal = sumXY / (sqrt(sumX) * sqrt(sumY));
			similarityMatrix += <highReq.req, lowReq.req, sumFinal>;			
		}
	}
	return similarityMatrix;
}

rel[str high, str low, num sim] createLinks
(rel[str high, str low, num sim] matrix, list[loc] high, list[loc] low, int tech) {
	rel[str high, str low, num sim] newMatrix = {};
	if (tech == 1) {
		newMatrix = matrix;
	}
	else if(tech == 2) {
		for(x <- high) {
			rel[num sim, str req] lows = invert(matrix[x.file]);
			counter = 4;
			while(counter > 0 && size(lows) > 0) {
				candidates = lows[max(lows.sim)];
				candidate = getOneFrom(candidates);
				newMatrix += <x.file, candidate, max(lows.sim)>;
				candidates -= candidate;
				lows -= <max(lows.sim), candidate>;
				counter -= 1;
			}					
		}	
	}
	else if(tech == 3) {
		for(m <- matrix) {
			if(m.sim >= 0.25) {
				newMatrix += m;
			}
		}
	}
	else if(tech == 4) {
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

list[str] convertFileToReqs(loc file) {
	reqs = readFileLines(file);	
	list[str] reqs1 = [];
	list[str] reqs2 = [];
	for(r <- reqs) {
		if(r != "%") {
			reqs1 += split("\t", r);
		}		
	}
	for(l <- reqs1) {
		reqs2 += split(" ", l);
	}
	for(l <- reqs2) {
		if(l == "") 
			reqs2 -= l;
	}
	return reqs2;
}


void main() {
	high = |project://traceability-links/data/high|.ls; 
	low = |project://traceability-links/data/low|.ls;
	techFile = |project://traceability-links/data/extra/tech.csv|;
	modeFile = |project://traceability-links/data/extra/mode.csv|;
	techSet = wordsInFile(techFile);
	modeSet = wordsInFile(modeFile);
	int tech = 1;
	int mode = 0;
	for(t <- techSet) {
		tech = toInt(t);
	}
	for(m <- modeSet) {
		mode = toInt(m);
	}
	println(tech);
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
	for(i <- [0 .. size(master)]) {
		frequencyMaster += highFrequencyMaster[i] + lowFrequencyMaster[i];
	}
	
	numReqs = size(high) + size(low);
	highReqs = normalizeVectorRelation(highReqs, frequencyMaster, numReqs);
	lowReqs = normalizeVectorRelation(lowReqs, frequencyMaster, numReqs);

	similarityMatrix = createSimilarityMatrix(highReqs, lowReqs);
	
	rel[str high, str low] masterLinks = {};
		for(m <- similarityMatrix) {
		masterLinks += <m.high, m.low>;
	}
	
	similarityMatrix = createLinks(similarityMatrix, high, low, tech);
	
	rel[str high, str low] similarityLinks = {};
	for(m <- similarityMatrix) {
		similarityLinks += <m.high, m.low>;
	}
	
	outputFile = |project://traceability-links/data/extra/output.csv|;
	writeFile(outputFile, "");
	
	if(mode == 0) {
		for(h <- high) {
			appendToFile(outputFile, "
			%
			" + h.file + "	");
			for(l <- similarityLinks[h.file]) {
				appendToFile(outputFile, l + " ");
			}	
		}
	}
	
	
	else {
		expertFile = |project://traceability-links/data/extra/handtrace.csv|;
		list[str] expertReqs = convertFileToReqs(expertFile);
		rel[str high, str low] expertLinks = {};
		str currentHigh = "";
		for(r <- expertReqs) {
			if(startsWith(r, "S")) {
				currentHigh = r;
			}
			else {
				expertLinks += <currentHigh, r>;
			}
		}
		
		appendToFile(outputFile, "trace-links identified by both the tool and manually:
");
		for(l <- similarityLinks & expertLinks) {
			appendToFile(outputFile, l);
			appendToFile(outputFile, "
");
		}		
		appendToFile(outputFile, "
"); 

		appendToFile(outputFile, "trace-links identified only by the tool:
");
		for(l <- similarityLinks - expertLinks) {
			appendToFile(outputFile, l);
			appendToFile(outputFile, "
");
		}		
		appendToFile(outputFile, "
");

		appendToFile(outputFile, "trace-links identified only manually:
");
		for(l <- expertLinks - similarityLinks) {
			appendToFile(outputFile, l);
			appendToFile(outputFile, "
");
		}		
		appendToFile(outputFile, "
");

		appendToFile(outputFile, "trace-links not identified by neither the tool or manually:
");
		for(l <- masterLinks - (similarityLinks + expertLinks)) {
			appendToFile(outputFile, l);
			appendToFile(outputFile, "
");
		}
		appendToFile(outputFile, "
");

		appendToFile(outputFile, "Recall: ");
		appendToFile(outputFile, percent(size(similarityLinks & expertLinks), size(expertLinks)));
		appendToFile(outputFile, "%
");
		appendToFile(outputFile, "Precision: ");
		appendToFile(outputFile, percent(size(similarityLinks & expertLinks), size(similarityLinks)));
		appendToFile(outputFile, "%
");
		appendToFile(outputFile, "Mistakes: ");
		appendToFile(outputFile, size(similarityLinks - expertLinks) + size(expertLinks - similarityLinks));	
	}
}
