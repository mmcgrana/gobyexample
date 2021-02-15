package ru.dfls.events {
	import flash.events.Event;	
	import flash.events.ErrorEvent;
	
	/**
	 * This event is usually dispatched if some error was thrown from an asynchronous code, i.e. there
	 * is no relevant user stack part to process the error. There is only one type of such event: 
	 * <code>ErrorEvent.ERROR</code> which is same as <code>flash.events.ErrorEvent.ERROR</code>.
	 * The only difference between <code>flash.events.ErrorEvent</code> and 
	 * <code>ru.dfls.events.ErrorEvent</code> is the capability of the latter to store the underlying cause
	 * (the <code>Error</code>).
	 * 
	 * @see flash.events.ErrorEvent
	 * @see Error
	 * @author dragonfly
	 */
	public class ErrorEvent extends flash.events.ErrorEvent {
		
		public static var ERROR : String = flash.events.ErrorEvent.ERROR;

		private var _error : Error;
		
		public function ErrorEvent(type : String, bubbles : Boolean = false, cancelable : Boolean = false, 
									text : String = "", error : Error = null) {
			super(type, bubbles, cancelable, text);
			_error = error;
		}
		
		public function get error() : Error {
			return _error;
		}
		
		public function set error(value : Error) : void {
			_error = value;
		}
		
		public override function toString() : String {
			return formatToString("ErrorEvent", "type", "bubbles", "cancelable", "eventPhase", "text", "error");
		}
		
		public override function clone() : Event {
			return new ru.dfls.events.ErrorEvent(type, bubbles, cancelable, text, error);
		}
		
	}
}
