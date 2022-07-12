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



num_target_items = 12;
const num_gram_filler = 12;
num_ungram_filler = 12;
block_size = 3;

experiment_condition = shuffle(["WH", "CNPC"])[0];
experiment_group = shuffle(["RP", "GAP"])[0];


console.log("condition", experiment_condition);
console.log("group", experiment_group);

/**
 * 
 * @param {*} items 
 * @param {*} group filter 1
 * @param {*} cond  filter 2
 * @returns filtered items
 * 
 * Method Name: Filt
 * Purpose: filter an array with two filter
 * 
 */

/* 
*Method Name: Filt
Purpose: filter an array with two filter
Paramter: group, cond (two filtering conditions)
*/
function filt(items, condition, num){
    return items.filter(item => (item.condition == condition /*&& item.condition == cond*/
                        && item.lex_items <= num))
}

//Desired stimuli list
experiment_stimuli = filt(all_stimuli, experiment_condition, 12);
//Desired grammatical filler
gram_filler = filt(all_stimuli, "FILL", 112);
//Desired ungrammatical filler
ungram_filler = filt(all_stimuli, "UNGRAM", 128);


//Testing logs
console.log("stimuli ", experiment_stimuli);
console.log("filler ", gram_filler);
console.log("ungram ", ungram_filler);


/**
 * Pseudo-block randomization of three lists of testing items
 * @param {*} list1 three lists of items
 * @param {*} list2 
 * @param {*} list3 
 * @returns An array of randomized items
 */
function pseudo_block(list1, list2, list3){
    output = [];
    num = l1.length;
    for(var i = 0; i < num; i++){
        block = [list1.pop(), list2.pop(), list3.pop()];
        block = shuffle(block);
        output.push(block);
    }
    return output;
}

final_stimuli = pseudo_block(experiment_stimuli, gram_filler, ungram_filler);
console.log("final stimuli", final_stimuli);

