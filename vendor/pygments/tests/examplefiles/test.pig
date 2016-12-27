/**
 *  This script is an example recommender (using made up data) showing how you might modify item-item links
 *  by defining similar relations between items in a dataset and customizing the change in weighting.
 *  This example creates metadata by using the genre field as the metadata_field.  The items with
 *  the same genre have it's weight cut in half in order to boost the signals of movies that do not have the same genre.
 *  This technique requires a customization of the standard GetItemItemRecommendations macro
 */
import 'recommenders.pig';



%default INPUT_PATH_PURCHASES '../data/retail/purchases.json'
%default INPUT_PATH_WISHLIST '../data/retail/wishlists.json'
%default INPUT_PATH_INVENTORY '../data/retail/inventory.json'
%default OUTPUT_PATH '../data/retail/out/modify_item_item'


/******** Custom GetItemItemRecommnedations *********/
define recsys__GetItemItemRecommendations_ModifyCustom(user_item_signals, metadata) returns item_item_recs {

    -- Convert user_item_signals to an item_item_graph
    ii_links_raw, item_weights   =   recsys__BuildItemItemGraph(
                                       $user_item_signals,
                                       $LOGISTIC_PARAM,
                                       $MIN_LINK_WEIGHT,
                                       $MAX_LINKS_PER_USER
                                     );
    -- NOTE this function is added in order to combine metadata with item-item links
        -- See macro for more detailed explination
    ii_links_metadata           =   recsys__AddMetadataToItemItemLinks(
                                        ii_links_raw,
                                        $metadata
                                    );

    /********* Custom Code starts here ********/

    --The code here should adjust the weights based on an item-item link and the equality of metadata.
    -- In this case, if the metadata is the same, the weight is reduced.  Otherwise the weight is left alone.
    ii_links_adjusted           =  foreach ii_links_metadata generate item_A, item_B,
                                        -- the amount of weight adjusted is dependant on the domain of data and what is expected
                                        -- It is always best to adjust the weight by multiplying it by a factor rather than addition with a constant
                                        (metadata_B == metadata_A ? (weight * 0.5): weight) as weight;


    /******** Custom Code stops here *********/

    -- remove negative numbers just incase
    ii_links_adjusted_filt = foreach ii_links_adjusted generate item_A, item_B,
                                      (weight <= 0 ? 0: weight) as weight;
    -- Adjust the weights of the graph to improve recommendations.
    ii_links                    =   recsys__AdjustItemItemGraphWeight(
                                        ii_links_adjusted_filt,
                                        item_weights,
                                        $BAYESIAN_PRIOR
                                    );

    -- Use the item-item graph to create item-item recommendations.
    $item_item_recs =  recsys__BuildItemItemRecommendationsFromGraph(
                           ii_links,
                           $NUM_RECS_PER_ITEM,
                           $NUM_RECS_PER_ITEM
                       );
};


/******* Load Data **********/

--Get purchase signals
purchase_input = load '$INPUT_PATH_PURCHASES' using org.apache.pig.piggybank.storage.JsonLoader(
                    'row_id: int,
                     movie_id: chararray,
                     movie_name: chararray,
                     user_id: chararray,
                     purchase_price: int');

--Get wishlist signals
wishlist_input =  load '$INPUT_PATH_WISHLIST' using org.apache.pig.piggybank.storage.JsonLoader(
                     'row_id: int,
                      movie_id: chararray,
                      movie_name: chararray,
                      user_id: chararray');


/******* Convert Data to Signals **********/

-- Start with choosing 1 as max weight for a signal.
purchase_signals = foreach purchase_input generate
                        user_id    as user,
                        movie_name as item,
                        1.0        as weight;


-- Start with choosing 0.5 as weight for wishlist items because that is a weaker signal than
-- purchasing an item.
wishlist_signals = foreach wishlist_input generate
                        user_id    as user,
                        movie_name as item,
                        0.5        as weight;

user_signals = union purchase_signals, wishlist_signals;


/******** Changes for Modifying item-item links ******/
inventory_input = load '$INPUT_PATH_INVENTORY' using org.apache.pig.piggybank.storage.JsonLoader(
                     'movie_title: chararray,
                      genres: bag{tuple(content:chararray)}');


metadata = foreach inventory_input generate
              FLATTEN(genres) as metadata_field,
              movie_title as item;
-- requires the macro to be written seperately
  --NOTE this macro is defined within this file for clarity
item_item_recs = recsys__GetItemItemRecommendations_ModifyCustom(user_signals, metadata);
/******* No more changes ********/


user_item_recs = recsys__GetUserItemRecommendations(user_signals, item_item_recs);

--Completely unrelated code stuck in the middle
data        =    LOAD 's3n://my-s3-bucket/path/to/responses'
                 USING org.apache.pig.piggybank.storage.JsonLoader();
responses   =    FOREACH data GENERATE object#'response' AS response: map[];
out         =    FOREACH responses
                 GENERATE response#'id' AS id: int, response#'thread' AS thread: chararray,
                          response#'comments' AS comments: {t: (comment: chararray)};
STORE out INTO 's3n://path/to/output' USING PigStorage('|');


/******* Store recommendations **********/

--  If your output folder exists already, hadoop will refuse to write data to it.

rmf $OUTPUT_PATH/item_item_recs;
rmf $OUTPUT_PATH/user_item_recs;

store item_item_recs into '$OUTPUT_PATH/item_item_recs' using PigStorage();
store user_item_recs into '$OUTPUT_PATH/user_item_recs' using PigStorage();

-- STORE the item_item_recs into dynamo
STORE item_item_recs
 INTO '$OUTPUT_PATH/unused-ii-table-data'
USING com.mortardata.pig.storage.DynamoDBStorage('$II_TABLE', '$AWS_ACCESS_KEY_ID', '$AWS_SECRET_ACCESS_KEY');

-- STORE the user_item_recs into dynamo
STORE user_item_recs
 INTO '$OUTPUT_PATH/unused-ui-table-data'
USING com.mortardata.pig.storage.DynamoDBStorage('$UI_TABLE', '$AWS_ACCESS_KEY_ID', '$AWS_SECRET_ACCESS_KEY');
