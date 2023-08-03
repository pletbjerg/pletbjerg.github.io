#include <sys/tree.h>
#include <stdio.h>
#include <stdlib.h>
#include <err.h>

/* 
 *	 Number of activities (n) / classrooms (m)
 */
size_t n,m;

/* 
 *	 Activities
 */

struct activity {
	size_t start_time;
	size_t finish_time;
};

/* All activities (at most 200,000) from problem statement) */
static struct activity activities[200000];

/* Compares two activities by finish time
 */
int 
cmp_activity(const void *a0, const void *a1) {
	struct activity const *new_a0 = (struct activity const *) a0;
	struct activity const *new_a1 = (struct activity const *) a1;
	return (new_a0->finish_time > new_a1->finish_time) 
		- (new_a0->finish_time < new_a1->finish_time);
}

void 
earliest_finish_time_first() {
	qsort(activities, n, sizeof(activities[0]), cmp_activity);
}

/* 
 *	 tc
 */
struct tc_node {
	RB_ENTRY(tc_node) entry;
	size_t tc;
	// If we wanted to know the schedule, then this
	// would have to be a list of the classrooms.
	size_t multiplicity;
};

/* Compares two nodes on the tc field where
 */
int 
cmp_tc_node(struct tc_node *n1, struct tc_node *n2) {
	return (n1->tc > n2->tc) - (n1->tc < n2->tc);
}

/* Non empty multiset which maps $$t_c$$ to its multiplicity
 */
RB_HEAD(tc_multiset, tc_node) head = RB_INITIALIZER(&head);
RB_GENERATE(tc_multiset, tc_node, entry, cmp_tc_node)

void 		tc_multiset_init();
void 		tc_multiset_insert(size_t);
struct tc_node *tc_multiset_pop_greatest_non_overlapping_tc(size_t);

// Create m (number of classrooms) tcs that start at time 0.
void 
tc_multiset_init() {
	tc_multiset_insert(0);
	struct tc_node *root = RB_ROOT(&head);

	if (root == NULL)
		err(1, "Impossible: failed tc_multiset_init");

	root->multiplicity = m;
}

void
tc_multiset_insert(size_t tc)
{
	struct tc_node elm = (struct tc_node){ .tc = tc };
	struct tc_node *lkup = RB_FIND(tc_multiset, &head, &elm);

	if (lkup == NULL) {
		struct tc_node * new_tc_node;
		if ((new_tc_node = malloc(sizeof(struct tc_node))) == NULL)
			err(1, NULL);
		new_tc_node->tc = tc;
		new_tc_node->multiplicity = 1;
		RB_INSERT(tc_multiset, &head, new_tc_node);

		// TODO: RB_INSERT returns NULL if the elemnt was 
		// inserted in the tree successfully, otherwise 
		// it returns a pointer to the element with the 
		// colliding key
	} else {
		++lkup->multiplicity;
	}

	return;
}


/* Finds greatest non overlapping node of a given activity's 
 * start time.
 */
struct tc_node *
tc_multiset_pop_greatest_non_overlapping_tc(size_t start_time)
{
	struct tc_node elm = (struct tc_node){ .tc = start_time };
        struct tc_node *lkup = RB_NFIND(tc_multiset, &head, &elm);

	if (lkup == NULL) {
		// The easy case when there is NO element greater than
		// or equal to the start_time i.e., everything in the set
		// is smaller, so just pick the largest thing.
		lkup = RB_MAX(tc_multiset, &head);
		if (lkup == NULL)
			err(1,"Impossible tc_multiset_pop_greatest_non_overlapping_tc empty tc_multiset");
	} else {
		// Otherwise, it's nonnull, and we have the least element
		// greater than or equal to the start_time, but we want 
		// the greatest element STRICTLY less than, so we need 
		// to go back one.
		lkup = RB_PREV(tc_multiset, &head, lkup);

		if (lkup == NULL)
			// if there is NO element strictly less than
			// start_time, just give up
			return NULL;
	}

	if (!--(lkup->multiplicity)) {
		// If the multiplicity is zero, we need 
		// to actually delete it
		if (RB_REMOVE(tc_multiset, &head, lkup) == NULL) {
			err(1, "RB_REMOVE failed");
		}
	}
	return lkup;
}
 
/* 
 *	 Main
 */
int 
main() {
        // Read the number of activities + classrooms
	scanf("%zu %zu", &n, &m);

        // Read all the activities in
	for (size_t i = 0; i < n; ++i)
		scanf("%zu %zu",
			&activities[i].start_time, 
			&activities[i].finish_time);
	
	// Sort the elements according by earliest finish time first
	earliest_finish_time_first();

	/*
         * The main processing of the algorithm
         */
	// Initializing
	size_t result = 0;
	tc_multiset_init();

	for (size_t i = 0; i < n; ++i) {
		size_t start_time = activities[i].start_time;
		size_t finish_time = activities[i].finish_time;
		struct tc_node *lkup = tc_multiset_pop_greatest_non_overlapping_tc(start_time);
		
		if (lkup == NULL)
			// we can't schedule this activity, so skip
			continue;
		
		// Move tc forward to the finish time.
		tc_multiset_insert(finish_time);

		// We can schedule activity i, so increment the count.
		++result;
	}

	printf("%zu", result);
}
