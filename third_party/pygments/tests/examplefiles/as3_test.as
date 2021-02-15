			import flash.events.MouseEvent;
			import com.example.programmingas3.playlist.PlayList;
			import com.example.programmingas3.playlist.Song;
			import com.example.programmingas3.playlist.SortProperty;

			// constants for the different "states" of the song form
			private static const ADD_SONG:uint = 1;
			private static const SONG_DETAIL:uint = 2;
			
			private var playList:PlayList = new PlayList.<T>();

			private function initApp():void
			{
				// set the initial state of the song form, for adding a new song
				setFormState(ADD_SONG);
				
				// prepopulate the list with a few songs
				playList.addSong(new Song("Nessun Dorma", "Luciano Pavarotti", 1990, "nessundorma.mp3", ["90's", "Opera"]));
				playList.addSong(new Song("Come Undone", "Duran Duran", 1993, "comeundone.mp3", ["90's", "Pop"]));
				playList.addSong(new Song("Think of Me", "Sarah Brightman", 1987, "thinkofme.mp3", ["Showtunes"]));
				playList.addSong(new Song("Unbelievable", "EMF", 1991, "unbelievable.mp3", ["90's", "Pop"]));

				songList.dataProvider = playList.songList;
			}


			private function sortList(sortField:SortProperty.<T>):void
			{
				// Make all the sort type buttons enabled.
				// The active one will be grayed-out below
				sortByTitle.selected = false;
				sortByArtist.selected = false;
				sortByYear.selected = false;

				switch (sortField)
				{
					case SortProperty.TITLE:
						sortByTitle.selected = true;
						break;
					case SortProperty.ARTIST:
						sortByArtist.selected = true;
						break;
					case SortProperty.YEAR:
						sortByYear.selected = true;
						break;
				}

				playList.sortList(sortField);
				
				refreshList();
			}


			private function refreshList():void
			{
				// remember which song was selected
				var selectedSong:Song = Song(songList.selectedItem);
				
				// re-assign the song list as the dataprovider to get the newly sorted list
				// and force the List control to refresh itself
				songList.dataProvider = playList.songList;
				
				// reset the song selection
				if (selectedSong != null)
				{
					songList.selectedItem = selectedSong;
				}
			}


			private function songSelectionChange():void
			{
				if (songList.selectedIndex != -1)
				{
					setFormState(SONG_DETAIL);
				}
				else
				{
					setFormState(ADD_SONG);
				}
			}


			private function addNewSong():void
			{
				// gather the values from the form and add the new song
				var title:String = newSongTitle.text;
				var artist:String = newSongArtist.text;
				var year:uint = newSongYear.value;
				var filename:String = newSongFilename.text;
				var genres:Array = newSongGenres.selectedItems;

				playList.addSong(new Song(title, artist, year, filename, genres));

				refreshList();
	
				// clear out the "add song" form fields
				setFormState(ADD_SONG);
			}


			private function songListLabel(item:Object):String
			{
				return item.toString();
			}


			private function setFormState(state:uint):void
			{
				// set the form title and control state
				switch (state)
				{
					case ADD_SONG:
						formTitle.text = "Add New Song";
						// show the submit button
						submitSongData.visible = true;
						showAddControlsBtn.visible = false;
						// clear the form fields
						newSongTitle.text = "";
						newSongArtist.text = "";
						newSongYear.value = (new Date()).fullYear;
						newSongFilename.text = "";
						newSongGenres.selectedIndex = -1;
						// deselect the currently selected song (if any)
						songList.selectedIndex = -1;
						break;
						
					case SONG_DETAIL:
						formTitle.text = "Song Details";
						// populate the form with the selected item's data
						var selectedSong:Song = Song(songList.selectedItem);
						newSongTitle.text = selectedSong.title;
						newSongArtist.text = selectedSong.artist;
						newSongYear.value = selectedSong.year;
						newSongFilename.text = selectedSong.filename;
						newSongGenres.selectedItems = selectedSong.genres;
						// hide the submit button
						submitSongData.visible = false;
						showAddControlsBtn.visible = true;
						break;
				}
			}

