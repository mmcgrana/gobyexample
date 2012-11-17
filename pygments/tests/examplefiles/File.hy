/*
 * This file is part of the Hybris programming language.
 *
 * Copyleft of Francesco Morucci aka merlok <merlok@ihteam.net>
 *
 * Hybris is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Hybris is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Hybris.  If not, see <http://www.gnu.org/licenses/>.
*/
import std.io.file;

class File {
	
	protected file, fileName, mode;

	public method File( fileName, mode ){
		me.fileName = fileName;
		me.mode = mode;
		me.file = fopen ( me.fileName, me.mode);
	}

	private method isBinary(){
		return me.mode.find("b") != false;
	}
	
	public method File ( file ){
		me.file = file;
	}

	private method __expire() {
		me.close();
	}
	
	public method close(){
		fclose( me.file );
	}
	
	public method readLine(){
		return line = fgets( me.file );
	}

	public method getFileName(){
		return me.fileName;
	}

	public method getSize(){
		return fsize( me.fileName );
	}

	public method getPosition(){
		return ftell( me.file );
	}
	
	public method readAll(){
		text = "";
		line = "";
		while ( ( line = fgets(me.file) ) != 0 ){
			text += line;
		}
		return text;
	}

	public method read(){
		byte = ' ';
		if ( fread( me.file, byte) > 0 ) {
			return byte;
		}
		else {
			return -1;
		}
	}

	public method read( bytes ) {
		word = "";
		byte = ' ';
		if ( fread( me.file, byte, bytes) > 0 ) {
			word += byte;
		}
		else {
			return -1;
		}
		return word;
	}

	public method read ( seek, seekType ){
		if ( me.seek( seek, seekType) == 0 ) {
			return -1;
		}

		return  me.read();
	}

	public method read ( bytes, seek, seekType ){
		if ( me.seek( seek, seekType) == 0 ) {
			return -1;
		}

		return  me.read( bytes );
	}

	public method readType ( type ){
		if ( me.isBinary() == false ) {
			return -1;
		}
		if ( fread (me.file, type ) > 0 ) {
			return type;
		} 
		else {
			return -1;
		}
	}

	operator >> ( object ){
		return me.readType(object);
	}

	public method readType ( type, bytes ){
		if ( me.isBinary() == false ) {
			return -1;
		}
		if ( fread (me.file, type, bytes ) > 0){
			return type;
		}
		else {
			return -1;
		}
	}

	public method readType ( type, seek, seekType ){
		if ( ( me.isBinary() == false ) | ( me.seek( seek, seekType) == 0 ) ) {
			return -1;
		}

		return me.readType( type );
	}
	
	public method  readType(  type, bytes, seek, seekType){
		if ( ( me.isBinary() == false ) | ( me.seek( seek, seekType) == 0 ) ) {
			return -1;
		}

		return me.readType( type, bytes );
	}
	
	public method write( data ){
		return fwrite( me.file, data );
	}

	operator << ( object ){
		return me.write(object);
	}

	public method write ( data, bytes ){
		return fwrite( me.file, data, bytes);
	}
	
	public method seek( pos, mode ){
		return fseek( me.file, pos, mode );
	}

	public method merge ( fileName ){
		text = file ( fileName );
		return me.write ( me.file, text );
	}
}
