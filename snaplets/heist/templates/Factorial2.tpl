<apply template="base">
<dfForm action="${postAction}">
	<table border="1">
    <tr>
      <th><dfLabel ref="number">Number</dfLabel></th>
      <td><dfInputText ref="number" size="5" /></td>
      <td><dfErrorList class="alert" ref="number" /></td>
    </tr>
	</table>
    <dfInputSubmit value="Ok" />
</dfForm>
</apply>

