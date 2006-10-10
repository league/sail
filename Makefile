
gridworld.zip: stdlib.scm grid-model.scm grid-view.scm grid-main.scm
	zip $@ $^

id3tree.zip: stdlib.scm id3tree.scm shrooms.data shrooms.names
	zip $@ $^

genetic.zip: gene-knapsack.scm gene-vec.scm gene-algo.scm stdlib.scm \
	gene-bot.scm gene-tree.scm grid-view.scm grid-model.scm
	zip $@ $^

roomba.zip: state-bot.scm grid-view.scm grid-model.scm stdlib.scm
	zip $@ $^

search.zip: search-bot.scm grid-view.scm grid-model.scm stdlib.scm \
	search-graph.scm sets.scm search-blocks.scm search-river.scm
	zip $@ $^
