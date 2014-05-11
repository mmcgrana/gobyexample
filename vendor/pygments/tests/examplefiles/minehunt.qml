 /****************************************************************************
 **
 ** Copyright (C) 2011 Nokia Corporation and/or its subsidiary(-ies).
 ** All rights reserved.
 ** Contact: Nokia Corporation (qt-info@nokia.com)
 **
 ** This file is part of the QtDeclarative module of the Qt Toolkit.
 **
 ** $QT_BEGIN_LICENSE:LGPL$
 ** GNU Lesser General Public License Usage
 ** This file may be used under the terms of the GNU Lesser General Public
 ** License version 2.1 as published by the Free Software Foundation and
 ** appearing in the file LICENSE.LGPL included in the packaging of this
 ** file. Please review the following information to ensure the GNU Lesser
 ** General Public License version 2.1 requirements will be met:
 ** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
 **
 ** In addition, as a special exception, Nokia gives you certain additional
 ** rights. These rights are described in the Nokia Qt LGPL Exception
 ** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
 **
 ** GNU General Public License Usage
 ** Alternatively, this file may be used under the terms of the GNU General
 ** Public License version 3.0 as published by the Free Software Foundation
 ** and appearing in the file LICENSE.GPL included in the packaging of this
 ** file. Please review the following information to ensure the GNU General
 ** Public License version 3.0 requirements will be met:
 ** http://www.gnu.org/copyleft/gpl.html.
 **
 ** Other Usage
 ** Alternatively, this file may be used in accordance with the terms and
 ** conditions contained in a signed written agreement between you and Nokia.
 **
 **
 **
 **
 **
 ** $QT_END_LICENSE$
 **
 ****************************************************************************/

 import QtQuick 1.0
 import "MinehuntCore" 1.0

 Item {
     id: field
     property int clickx: 0
     property int clicky: 0

     width: 450; height: 450

     Image { source: "MinehuntCore/pics/background.png"; anchors.fill: parent; fillMode: Image.Tile }

     Grid {
         anchors.horizontalCenter: parent.horizontalCenter
         columns: 9; spacing: 1

         Repeater {
             id: repeater
             model: tiles
             delegate: Tile {}
         }
     }

     Row {
         id: gamedata
         x: 20; spacing: 20
         anchors.bottom: field.bottom; anchors.bottomMargin: 15

         Image {
             source: "MinehuntCore/pics/quit.png"
             scale: quitMouse.pressed ? 0.8 : 1.0
             smooth: quitMouse.pressed
             y: 10
             MouseArea {
                 id: quitMouse
                 anchors.fill: parent
                 anchors.margins: -20
                 onClicked: Qt.quit()
             }
         }
         Column {
             spacing: 2
             Image { source: "MinehuntCore/pics/bomb-color.png" }
             Text { anchors.horizontalCenter: parent.horizontalCenter; color: "white"; text: numMines }
         }

         Column {
             spacing: 2
             Image { source: "MinehuntCore/pics/flag-color.png" }
             Text { anchors.horizontalCenter: parent.horizontalCenter; color: "white"; text: numFlags }
         }
     }

     Image {
         anchors.bottom: field.bottom; anchors.bottomMargin: 15
         anchors.right: field.right; anchors.rightMargin: 20
         source: isPlaying ? 'MinehuntCore/pics/face-smile.png' :
         hasWon ? 'MinehuntCore/pics/face-smile-big.png': 'MinehuntCore/pics/face-sad.png'

         MouseArea { anchors.fill: parent; onPressed: reset() }
     }
     Text {
         anchors.centerIn: parent; width: parent.width - 20
         horizontalAlignment: Text.AlignHCenter
         wrapMode: Text.WordWrap
         text: "Minehunt demo has to be compiled to run.\n\nPlease see README."
         color: "white"; font.bold: true; font.pixelSize: 14
         visible: tiles == undefined
     }

 }
