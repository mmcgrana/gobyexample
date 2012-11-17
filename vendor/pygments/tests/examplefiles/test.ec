namespace gui;

import "Window"

public struct AnchorValue
{
   AnchorValueType type;

   union
   {
      int distance;
      float percent;      
   };
   property int
   {
      set { distance = value; type = offset; }
      get { return distance; }
   }
   property double
   {
      set { percent = (float) value; type = relative; }
      get { return (double) percent; }
   }

   char * OnGetString(char * stringOutput, void * fieldData, bool * needClass)
   {
      if(type == offset)
      {
         sprintf(stringOutput, "%d", distance);
      }
      else if(type == relative)
      {
         int c;
         int last = 0;
         sprintf(stringOutput, "%f", percent);
         c = strlen(stringOutput)-1;
         for( ; c >= 0; c--)
         {
            if(stringOutput[c] != '0') 
               last = Max(last, c);
            if(stringOutput[c] == '.')
            {
               if(last == c)
               {
                  stringOutput[c+1] = '0';
                  stringOutput[c+2] = 0;
               }
               else
                  stringOutput[last+1] = 0;
               break;
            }
         }
      }
      if(needClass) *needClass = false;
      return stringOutput;
   }

   bool OnGetDataFromString(char * stringOutput)
   {
      char * end;
      if(strchr(stringOutput, '.'))
      {
         float percent = (float)strtod(stringOutput, &end);
         
         if(end != stringOutput)
         {
            this.percent = percent;
            type = relative;
            return true;
         }
      }
      else if(stringOutput[0])
      {
         int distance = strtol(stringOutput, &end, 0);
         if(end != stringOutput)
         {
            this.distance = distance;
            type = offset;
            return true;
         }
      }
      else
      {
         distance = 0;
         type = 0;
      }
      return false;
   }
};

public struct MiddleAnchorValue
{
   AnchorValueType type;

   union
   {
      int distance;
      float percent;      
   };
   property int
   {
      set { distance = value; type = none; }
      get { return distance; }
   }
   property double
   {
      set { percent = (float) value; type = middleRelative; }
      get { return (double) percent; }
   }

   char * OnGetString(char * stringOutput, void * fieldData, bool * needClass)
   {
      if(type == middleRelative)
      {
         int c;
         int last = 0;
         sprintf(stringOutput, "%f", percent);
         c = strlen(stringOutput)-1;
         for( ; c >= 0; c--)
         {
            if(stringOutput[c] != '0') 
               last = Max(last, c);
            if(stringOutput[c] == '.')
            {
               if(last == c)
               {
                  stringOutput[c+1] = '0';
                  stringOutput[c+2] = 0;
               }
               else
                  stringOutput[last+1] = 0;
               break;
            }
         }
      }
      else if(type == none && distance)
      {
         sprintf(stringOutput, "%d", distance);
      }
      if(needClass) *needClass = false;
      return stringOutput;
   }

   bool OnGetDataFromString(char * stringOutput)
   {
      if(strchr(stringOutput, '.'))
      {
         percent = (float)strtod(stringOutput, null);
         type = middleRelative;
      }
      else
      {
         distance = strtol(stringOutput, null, 0);
         type = none;
      }
      return true;
   }
};

public enum AnchorValueType { none, offset, relative, middleRelative, cascade, vTiled, hTiled };

public struct Anchor
{
   union { AnchorValue left; MiddleAnchorValue horz; };
   union { AnchorValue top; MiddleAnchorValue vert; };
   AnchorValue right, bottom;      

   char * OnGetString(char * stringOutput, void * fieldData, bool * needClass)
   {
      char tempString[256];
      char * anchorValue;
      bool subNeedClass;

      tempString[0] = '\0';
      anchorValue = left.OnGetString(tempString, null, &subNeedClass);
      if(anchorValue[0]) { if(stringOutput[0]) strcat(stringOutput, ", "); strcat(stringOutput, "left = "); strcat(stringOutput, anchorValue); }
      
      //if(((!left.type && !right.type) && horz.distance) || horz.type == middleRelative)
      if(!right.type && ((!left.type && horz.distance) || horz.type == middleRelative))
      {
         tempString[0] = '\0';
         anchorValue = horz.OnGetString(tempString, null, &subNeedClass);
         if(anchorValue[0]) { if(stringOutput[0]) strcat(stringOutput, ", "); strcat(stringOutput, "horz = "); strcat(stringOutput, anchorValue); }
      }
      
      tempString[0] = '\0';
      anchorValue = top.OnGetString(tempString, null, &subNeedClass);
      if(anchorValue[0]) { if(stringOutput[0]) strcat(stringOutput, ", "); strcat(stringOutput, "top = "); strcat(stringOutput, anchorValue); }
      
      tempString[0] = '\0';
      anchorValue = right.OnGetString(tempString, null, &subNeedClass);
      if(anchorValue[0]) { if(stringOutput[0]) strcat(stringOutput, ", "); strcat(stringOutput, "right = "); strcat(stringOutput, anchorValue); }

      // if(((!top.type && !bottom.type) && vert.distance) || vert.type == middleRelative)
      if(!bottom.type && ((!top.type && vert.distance) || vert.type == middleRelative))
      {
         tempString[0] = '\0';
         anchorValue = vert.OnGetString(tempString, null, &subNeedClass);
         if(anchorValue[0]) { if(stringOutput[0]) strcat(stringOutput, ", "); strcat(stringOutput, "vert = "); strcat(stringOutput, anchorValue); }
      }
      
      tempString[0] = '\0';
      anchorValue = bottom.OnGetString(tempString, null, &subNeedClass);
      if(anchorValue[0]) { if(stringOutput[0]) strcat(stringOutput, ", "); strcat(stringOutput, "bottom = "); strcat(stringOutput, anchorValue); }
      
      return stringOutput;
   }

   bool OnGetDataFromString(char * string)
   {
      this = Anchor {};
      return class::OnGetDataFromString(string);
   }

   bool OnSaveEdit(DropBox dropBox, void * object)
   {
      return dropBox.Save();
   }

   Window OnEdit(Window listBox, Window master, int x, int y, int w, int h, Window control)
   {
      char * string = "";
      AnchorDropBox comboBox
      {
         editText = true;
         parent = listBox;
         master = master;
         position = Point { x, y };
         //clientSize = Size { h = h };
         //size.w = w;
         size = { w, h };
         anchorValue = this;
         control = control;
         borderStyle = 0;
      };
      
      comboBox.Create();

      {
         char tempString[MAX_F_STRING] = "";
         bool needClass = false;
         char * result = OnGetString(tempString, null, &needClass);
         if(result) string = result;
      }
      comboBox.contents = string;
      return comboBox;
   }
};

private class AnchorButton : Button
{
   toggle = true, bevel = false;

   void OnRedraw(Surface surface)
   {
      int cw = clientSize.w;
      int ch = clientSize.h;

      surface.SetForeground(black);
      if(checked)
      {
         surface.SetBackground(Color { 85,85,85 });
         surface.Area(0,0, cw-1, ch-1);
      }
      else
         surface.LineStipple(0xAAAA);

      surface.Rectangle(0,0,cw-1,ch-1);

      if(active)
      {
         surface.LineStipple(0xAAAA);
         surface.Rectangle(2,2,cw-3,ch-3);
      }
   }

   bool AnchorEditor::NotifyClicked(Button button, int x, int y, Modifiers mods)
   {
      AnchorDropBox anchorDropBox = (AnchorDropBox)master;
      Anchor anchor = anchorDropBox.anchorValue;
      Window control = anchorDropBox.control;
      DataBox dropMaster = (DataBox)anchorDropBox.master;
      int id = button.id;

      switch(id)
      {
         case 0: anchor.left.type   = button.checked ? offset : none; break;
         case 1: anchor.top.type    = button.checked ? offset : none; break;
         case 2: anchor.right.type  = button.checked ? offset : none; break;
         case 3: anchor.bottom.type = button.checked ? offset : none; break;
      }

      if(anchor.horz.type == middleRelative && (id == 0 || id == 2))
      {
         anchorDropBox.relButtons[0].checked = false;
         anchorDropBox.relButtons[2].checked = false;
      }
      if(anchor.vert.type == middleRelative && (id == 1 || id == 3))
      {
         anchorDropBox.relButtons[1].checked = false;
         anchorDropBox.relButtons[3].checked = false;
      }
      anchorDropBox.relButtons[id].checked = false;

      //anchor.horz.type = none;
      //anchor.vert.type = none;

      {
         int vpw, vph;
         int x,y,w,h;
         Window parent = control.parent;

         // Fix Anchor
         x = control.position.x;
         y = control.position.y;
         w = control.size.w;
         h = control.size.h;

         vpw = parent.clientSize.w;
         vph = parent.clientSize.h;
         if(control.nonClient)
         {
            vpw = parent.size.w;
            vph = parent.size.h;
         }
         else if(((BorderBits)control.borderStyle).fixed)
         {
            if(!control.dontScrollHorz && parent.scrollArea.w) vpw = parent.scrollArea.w;
            if(!control.dontScrollVert && parent.scrollArea.h) vph = parent.scrollArea.h;
         }

         if(anchor.left.type == offset) anchor.left.distance = x;
         else if(anchor.left.type == relative) anchor.left.percent = (float)x / vpw;
         if(anchor.top.type == offset) anchor.top.distance = y;
         else if(anchor.top.type == relative) anchor.top.percent = (float)y / vph;
         if(anchor.right.type == offset) anchor.right.distance = vpw - (x + w);
         //else if(anchor.right.type == relative) anchor.right.percent = (float) (x + w) / vpw;
         else if(anchor.right.type == relative) anchor.right.percent = (float) (vpw - (x + w)) / vpw;
         if(anchor.bottom.type == offset) anchor.bottom.distance = vph - (y + h);
         //else if(anchor.bottom.type == relative) anchor.bottom.percent = (float) (y + h) / vph;
         else if(anchor.bottom.type == relative) anchor.bottom.percent = (float) (vph - (y + h)) / vph;

         if(!anchor.left.type && !anchor.right.type)
         {
            anchor.horz.distance = (x + w / 2) - (vpw / 2);
            //anchor.horz.type = anchor.horz.distance ? offset : 0;
         }
         else if(anchor.horz.type == middleRelative) anchor.horz.percent = (float) ((x + w / 2) - (vpw / 2)) / vpw;
         if(!anchor.top.type && !anchor.bottom.type)
         {
            anchor.vert.distance = (y + h / 2) - (vph / 2);
            //anchor.vert.type = anchor.vert.distance ? offset : 0;
         }
         else if(anchor.vert.type == middleRelative) anchor.vert.percent = (float)((y + h / 2) - (vph / 2)) / vph;
      }

      {
         char tempString[1024] = "";
         bool needClass = false;
         char * string = anchor.OnGetString(tempString, null, &needClass);
         anchorDropBox.contents = string;
      }

      dropMaster.SetData(&anchor, false);
      anchorDropBox.anchorValue = anchor;
      return true;
   }
}

private class AnchorRelButton : Button
{
   toggle = true;
   bevel = false;
   text = "%";
   //bevelOver = true;

   void OnRedraw(Surface surface)
   {
      int cw = clientSize.w;
      int ch = clientSize.h;
      
      if(checked)
      {
         surface.SetForeground(black);
      }
      else
      {
         surface.SetForeground(Color{170,170,170});
      }
      surface.WriteText(5,2, "%", 1);

      if(active)
      {
         surface.LineStipple(0xAAAA);
         surface.Rectangle(3,3,cw-4,ch-4);
      }
   }

   bool AnchorEditor::NotifyClicked(Button button, int x, int y, Modifiers mods)
   {
      AnchorDropBox anchorDropBox = (AnchorDropBox)master;
      Anchor anchor = anchorDropBox.anchorValue;
      Window control = anchorDropBox.control;
      DataBox dropMaster = (DataBox)anchorDropBox.master;
      int id = button.id;

      if((id == 0 || id == 2) && ((!anchor.left.type && !anchor.right.type) || anchor.left.type == middleRelative))
      {
         if(button.checked) anchor.horz.type = middleRelative; else anchor.horz.type = none;
         anchorDropBox.relButtons[(id + 2)%4].checked = button.checked;
      }
      else if((id == 1 || id == 3) && ((!anchor.top.type && !anchor.bottom.type) || anchor.top.type == middleRelative))
      {
         if(button.checked) anchor.vert.type = middleRelative; else anchor.vert.type = none;
         anchorDropBox.relButtons[(id + 2)%4].checked = button.checked;
      }
      else
      {
         switch(id)
         {
            case 0: anchor.left.type   = button.checked ? relative : (anchor.left.type   ? offset : none); break;
            case 1: anchor.top.type    = button.checked ? relative : (anchor.top.type    ? offset : none); break;
            case 2: anchor.right.type  = button.checked ? relative : (anchor.right.type  ? offset : none); break;
            case 3: anchor.bottom.type = button.checked ? relative : (anchor.bottom.type ? offset : none); break;
         }
         anchorDropBox.buttons[id].checked = true;
         if(anchor.horz.type == middleRelative) anchor.horz.type = none;
         if(anchor.vert.type == middleRelative) anchor.vert.type = none;
      }

      {
         int vpw, vph;
         int x,y,w,h;
         Window parent = control.parent;

         // Fix Anchor
         x = control.position.x;
         y = control.position.y;
         w = control.size.w;
         h = control.size.h;

         vpw = parent.clientSize.w;
         vph = parent.clientSize.h;
         if(control.nonClient)
         {
            vpw = parent.size.w;
            vph = parent.size.h;
         }
         else if(((BorderBits)control.borderStyle).fixed)
         {
            if(!control.dontScrollHorz && parent.scrollArea.w)  vpw = parent.scrollArea.w;
            if(!control.dontScrollVert && parent.scrollArea.h) vph = parent.scrollArea.h;
         }

         if(anchor.left.type == offset) anchor.left.distance = x;
         else if(anchor.left.type == relative) anchor.left.percent = (float)x / vpw;
         if(anchor.top.type == offset) anchor.top.distance = y;
         else if(anchor.top.type == relative) anchor.top.percent = (float)y / vph;
         if(anchor.right.type == offset) anchor.right.distance = vpw - (x + w);
         //else if(anchor.right.type == relative) anchor.right.percent = (float) (x + w) / vpw;
         else if(anchor.right.type == relative) anchor.right.percent = (float) (vpw - (x + w)) / vpw;
         if(anchor.bottom.type == offset) anchor.bottom.distance = vph - (y + h);
         //else if(anchor.bottom.type == relative) anchor.bottom.percent = (float) (y + h) / vph;
         else if(anchor.bottom.type == relative) anchor.bottom.percent = (float) (vph - (y + h)) / vph;

         if(!anchor.left.type && !anchor.right.type)
         {
            anchor.horz.distance = (x + w / 2) - (vpw / 2);
            //anchor.horz.type = anchor.horz.distance ? offset : none;
         }
         else if(anchor.horz.type == middleRelative) anchor.horz.percent = (float) ((x + w / 2) - (vpw / 2)) / vpw;
         if(!anchor.top.type && !anchor.bottom.type) 
         {
            anchor.vert.distance = (y + h / 2) - (vph / 2);
            //anchor.vert.type = anchor.vert.distance ? offset : none;
         }
         else if(anchor.vert.type == middleRelative) anchor.vert.percent = (float)((y + h / 2) - (vph / 2)) / vph;
      }

      {
         char tempString[1024] = "";
         bool needClass = false;
         char * string = anchor.OnGetString(tempString, null, &needClass);
         anchorDropBox.contents = string;
      }

      dropMaster.SetData(&anchor, false);
      anchorDropBox.anchorValue = anchor;
      return true;
   }
}

private class AnchorEditor : Window
{
   interim = true;
   borderStyle = deepContour;
   size.h = 92;

   bool OnKeyDown(Key key, unichar ch)
   {
      if(key == escape)
         return master.OnKeyDown(key, ch);
      return true;
   }
}

private class AnchorDropBox : DropBox
{
   Anchor anchorValue;
   Window control;
   Button relButtons[4], buttons[4];

   AnchorEditor anchorEditor
   {
      master = this;
      autoCreate = false;
   };

   Window OnDropDown()
   {
      int c;
      Button
      {
         anchorEditor,
         anchor = Anchor { left = 28, top = 28, right = 28, bottom = 28 },
         inactive = true, disabled = true
      };
      for(c = 0; c<4; c++)
      {
         Button button = buttons[c] = AnchorButton 
         { 
            anchorEditor, id = c,
            size = Size { (c%2)?10:28, (c%2)?28:10 }
         };
         Button relButton = relButtons[c] = AnchorRelButton
         {
            anchorEditor, id = c;
         };

         switch(c)
         {
            case 0:
               if(anchorValue.left.type && anchorValue.left.type != middleRelative) button.checked = true;
               if(anchorValue.left.type == relative || anchorValue.horz.type == middleRelative) relButton.checked = true;
               
               button.anchor = Anchor { left = 0 };
               relButton.anchor = Anchor { left = 5, vert = 16 };
               break;
            case 1:
               if(anchorValue.top.type && anchorValue.top.type != middleRelative) button.checked = true;
               if(anchorValue.top.type == relative || anchorValue.vert.type == middleRelative) relButton.checked = true;

               button.anchor = Anchor { top = 0 };
               relButton.anchor = Anchor { top = 5, horz = 16 };
               break;
            case 2: 
               if(anchorValue.right.type && anchorValue.right.type != middleRelative) button.checked = true;
               if(anchorValue.right.type == relative || anchorValue.horz.type == middleRelative) relButton.checked = true;
               
               button.anchor = Anchor { right = 0 };
               relButton.anchor = Anchor { right = 5, vert = 16 };
               break;
            case 3: 
               if(anchorValue.bottom.type && anchorValue.bottom.type != middleRelative) button.checked = true;
               if(anchorValue.bottom.type == relative || anchorValue.vert.type == middleRelative) relButton.checked = true;

               button.anchor = Anchor { bottom = 0 };
               relButton.anchor = Anchor { bottom = 5, horz = 16 };
               break;
         }
      }
      anchorEditor.Create();
      return anchorEditor;
   }
      
   void OnCloseDropDown(Window anchorEditor)
   {
      // TOFIX: Patch for update bug
      master.Update(null);
      anchorEditor.Destroy(0);
   }

   bool DataBox::NotifyTextEntry(AnchorDropBox dropBox, char * string, bool save)
   {
      Anchor anchor = dropBox.anchorValue;
      Window control = dropBox.control;

      if(save)
      {
         if(anchor.OnGetDataFromString(string))
         {
            SetData(&anchor, false);
            dropBox.anchorValue = anchor;
         }
      }
      else
      {
         char tempString[1024] = "";
         bool needClass = false;
         char * string = anchor.OnGetString(tempString, null, &needClass);
         dropBox.contents = string;
      }
      return true;
   }
}
