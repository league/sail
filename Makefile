
gridworld.zip: stdlib.scm grid-model.scm grid-view.scm grid-main.scm
	zip $@ $^

id3tree.zip: stdlib.scm id3tree.scm shrooms.data shrooms.names
	zip $@ $^

genetic.zip: gene-knapsack.scm gene-vec.scm gene-algo.scm stdlib.scm \
	gene-bot.scm gene-tree.scm grid-view.scm grid-model.scm
	zip $@ $^
