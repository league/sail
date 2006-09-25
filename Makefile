
gridworld.zip: stdlib.scm grid-model.scm grid-view.scm grid-main.scm
	zip $@ $^

id3tree.zip: stdlib.scm id3tree.scm shrooms.data shrooms.names
	zip $@ $^
