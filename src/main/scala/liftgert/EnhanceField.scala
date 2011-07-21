/*

	Package "liftgert"

	Gert's reusable Lift-related code.

	2011-06-11 Gert Rieger
	-	Created under:
			/Users/Shared/Development/NetBeans-Gert/With-NetBeans-6.8/ScalaGert/Precious/Lift23/src/main/scala/liftgert/

	-	Object "EnhanceField"

		Support for adding date pickers and the like to form fields, LiftScreen fields, and Mappers:

		+	Generates field IDs so JavaScript (like "jQuery" calls) can refer to the field.

		+	Keeps track of file includes (like "date.js", "jquery.bgiframe.js", "jquery.datePicker.js", "datePicker.css")
			so they are made only once on each HTML page.

	2011-07-07 Gert Rieger
	-	Modified under
			/Users/Shared/Development/NetBeans-Gert/With-NetBeans-6.8/ScalaGert/Precious/Forum1/src/main/scala/liftgert/
	-	Added class MapperHelper (function save_and_return_obj).
 
*/

package liftgert

import scala.xml._
import scala.collection._

import net.liftweb.http._
import net.liftweb.util.Helpers._
import net.liftweb.mapper._



private case class State (
	var i_field_id: Int,
	val mss_file_includeHtml: mutable.Map[String, String]
)

object EnhanceField {
	private object stateVar extends RequestVar[State] (
		State(
			0,
			mutable.Map[String, String]()
		)
	)
	
	def nextFieldId: String = {
	/*
		Generates the id for the next enhanced field.
	*/
		val state = stateVar.is
		state.i_field_id += 1
		"enhanced_" + state.i_field_id
	}

	def apply (
		in_fieldHtml: NodeSeq,
		in_f_id_enhancerHtml: (String) => NodeSeq,
		in_at_file_includeHtml: Pair[String, NodeSeq]*
	): NodeSeq = {
	/*
		Enhances a field.
		
		Takes:
		1.	The HTML for an input field, e.g. as generatde by SHtml.text.
		2.	A code fragment takes the field ID we generate and provides the HTML that enhances the field.
			For example:

			id =>
				<script type="text/javascript">
					Date.format = 'yyyy/mm/dd';
					jQuery(function () {{jQuery('#{id}').datePicker({{startDate:'00010101', clickInput:true}});}})
				</script>

		3.	A list of zero or more (filename, HTML) pairs.
			The HTML is supposed to include the named file, e.g. a .js or .css file.
			Here's an example for such a list:

			"date.js" ->
				<script type="text/javascript" src="/scripts/date.js"></script>,
			"jquery.bgiframe.js" ->
				<!--[if IE]>
					<script type="text/javascript" src="/scripts/jquery.bgiframe.js"> </script>
				<![endif]-->,
			"jquery.datePicker.js" ->
				<script type="text/javascript" src="/scripts/jquery.datePicker.js"></script>,
			"datePicker.css" ->
				<link rel="stylesheet" type="text/css" href="/style/datePicker.css" />

		This function will:
		
		1.	Generate a unique field ID.
		2.	Find an element with id = "enhanced" in the field HTML, and change its ID to the generated ID.
		3.	For each file that wasn't included on the same page before, appends its inclusion HTML to the field HTML.
		4.	Call the HTML enhancer function, passing the generated ID, and append the returned HTML to the field HTML.
		5.	Return the resulting HTML.
		
		Note:
		If the same file is included again, but with a different inclusion HTML,
		then an error message will be added the the resulting HTML.
	 */

		val state = stateVar.is
		
		// Generate a new unique field ID:
		
		state.i_field_id += 1
		val s_id = "enhanced_" + state.i_field_id
		
		// Put the generated field ID into the field HTML:
		
		val modifiedFieldHtml = ("#enhanced [id]" #> s_id)(in_fieldHtml)
		
		// Generate the needed include HTMLs:
		
		val includeHtmls =
			in_at_file_includeHtml.flatMap(
				t_file_includeHtml => {
					// Generate the needed include HTML for each file:

					val s_file = t_file_includeHtml._1
					val includeHtml = t_file_includeHtml._2
					val s_includeHtml = includeHtml.toString

					state.mss_file_includeHtml.get(s_file) match {
						// Get the old include HTML, if any:

						case None => {
							// We haven't included this file before:
							
							state.mss_file_includeHtml(s_file) = s_includeHtml	// Remember we've included it

							if ( s_includeHtml.contains(s_file) ) {
								// OK, the include HTML is well behaved:
								includeHtml	// Include it
							} else {
								// Strange, the include HTML doesn't contain the
								// name of the file that it's supposed to include:
								(
									includeHtml	// Include it, but report an error:
								++
									<div id="lift__noticesContainer__">
										<div id="lift__noticesContainer___error">
											<ul>
												<li>
													Error in EnhanceField:<br/>
													As a plausibility check, the include HTML for some 
													file is required to actually contain the file's name.<br/>
													For the file "{s_file}", that is not the case.<br/>
													This is probably a programming mistake.
												</li>
											</ul>
										</div>
									</div>
								)
							} // Strange, the include HTML doesn't contain the [...]
						} // case None

						case Some(s_includeHtmlOld) => {
							// We've included this file before:
							
							/*
								Note:
								We used to store and compare the NodeSeq, but that method wasn't able
								to detect the HTML difference we tested with (an added attribute).
								Storing and comparing the String versions instead seems to work better.
							*/
							
							if ( s_includeHtmlOld == s_includeHtml ) {
								// Using the same HTML fragment - ok:
								println("DEBUGGING EnhanceField: \"" + s_file + "\" was included before with the same HTML: " + s_includeHtml)
								xml.NodeSeq.Empty
							} else {
								// Using a different HTML fragment - report an error:
								println("DEBUGGING EnhanceField: \"" + s_file + "\" was included before with a different HTML: " + s_includeHtml)
								<div id="lift__noticesContainer__">
									<div id="lift__noticesContainer___error">
										<ul>
											<li>
												Error in EnhanceField:<br/>
												As a plausibility check, when a file is included more 
												than once, the include HTML must always be identical.<br/>
												For the file "{s_file}", that is not the case.<br/>
												This is probably a programming mistake.
											</li>
										</ul>
									</div>
								</div>
							} // Using a different HTML fragment - report an error
						} // case Some(includeHtmlOld)
					} // Get the old include HTML, if any
				} // Generate the needed include HTML for each file
			) // in_at_file_includeHtml.flatMap
		
		// Create the enhancer HTML:
		
		val enhancerHtml = in_f_id_enhancerHtml(s_id)
		
		// And return it all:
		
		modifiedFieldHtml ++ includeHtmls ++ enhancerHtml
	} // def apply

} // object EnhanceField



class MapperHelper[MAPPER <: _root_.net.liftweb.mapper.LongKeyedMapper[MAPPER] with IdPK] (val o_mapper: MAPPER) {
	def save_and_return_obj =
		// Convenience method for all Mappers with IdPK:
		// Save the mapper to the database, and return the object in a single step.
		// This is used in InitTablesIfNeeded, for example.
		if ( o_mapper.save )
			o_mapper
		else
			throw new Exception("MapperHelper.save_and_return_obj: Could not save the mapper to the database")
} // class MapperHelper



object MapperHelper {
	implicit def toMapperHelper[MAPPER <: _root_.net.liftweb.mapper.LongKeyedMapper[MAPPER] with IdPK] (o_mapper: MAPPER) =
		new MapperHelper[MAPPER](o_mapper)
}
