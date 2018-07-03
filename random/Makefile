# path to where 1B Word Benchmark files were extracted.
OBWB_ROOT ?= obwb
OBWB_TRAIN = $(OBWB_ROOT)/training-monolingual.tokenized.shuffled
OBWB_TEST = $(OBWB_ROOT)/heldout-monolingual.tokenized.shuffled

# Number of lines to train from heldout set.
# We will create a corpus with given number of lines.
# The original data contains about slightly more than 30.3M lines,
# the value of 31000000 and above will include all the corpus.
NUM_LINES ?= 10

# n-grams order. We will record $(ORDER)-1 of words as a context for prediction of last word.
ORDER ?= 3

# The corpus and derived file names. All include lines' count in their names so that we won't repeat computations.
CORPUS_FN = corpus-$(NUM_LINES)
WORDS_INDICES_FN = words-indices-$(NUM_LINES)
COUNTED_NGRAMS_FN = counted-$(ORDER)-grams-$(NUM_LINES)

all: Ops.hs $(COUNTED_NGRAMS_FN)

$(COUNTED_NGRAMS_FN): preprocess-train $(WORDS_INDICES_FN) $(CORPUS_FN)
	./preprocess-train $(ORDER) $(WORDS_INDICES_FN) $(CORPUS_FN) $(COUNTED_NGRAMS_FN)

$(WORDS_INDICES_FN): get-words-order $(CORPUS_FN)
	./get-words-order <$(CORPUS_FN) >$(WORDS_INDICES_FN)-tmp
	mv -f $(WORDS_INDICES_FN)-tmp $(WORDS_INDICES_FN)

$(CORPUS_FN):
	cat $(OBWB_TRAIN)/* | head -n $(NUM_LINES) >$(CORPUS_FN)

get-words-order: WordsOrder.hs
	ghc -Wno-tabs -odir objs -hidir objs -o get-words-order WordsOrder.hs -main-is WordsOrder.main

preprocess-train: PreprocessTrain.hs
	ghc -Wno-tabs -odir objs -hidir objs -o preprocess-train PreprocessTrain.hs -main-is PreprocessTrain.main

Ops.hs: GenOps.hs ../series/F.hs
	runghc -i../series -Wno-tabs GenOps.hs
