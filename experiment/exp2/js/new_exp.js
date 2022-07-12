var order = 1;

// I don't know why this exists, but it seems to work
// and we make use of it
var shuffle = function (array) {

	var currentIndex = array.length;
	var temporaryValue, randomIndex;

	// While there remain elements to shuffle...
	while (0 !== currentIndex) {
		// Pick a remaining element...
		randomIndex = Math.floor(Math.random() * currentIndex);
		currentIndex -= 1;

		// And swap it with the current element.
		temporaryValue = array[currentIndex];
		array[currentIndex] = array[randomIndex];
		array[randomIndex] = temporaryValue;
	}

	return array;

};

// experiment parameters

num_test_items = 12; // how many target items in the exposure phase?
num_g_items = 10; // how many target items in the test phase?
num_unique_lex_items = num_exposure_items + num_test_items;
num_fillers = 11; /* half the number of target items. Each type of filler
                  will have this many items */
block_size = 4;
num_exposure_blocks = num_exposure_items/2; // 2 target items per block
num_test_blocks = num_test_items/2; // 2 target items per block