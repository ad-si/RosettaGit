+++
title = "100 doors/XSLT"
description = ""
date = 2010-12-03T15:08:50Z
aliases = []
[extra]
id = 8918
[taxonomies]
categories = []
tags = []
+++


```xslt

<xsl:template name="HundredDoors">
	<xsl:param name="Doors">0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000</xsl:param>
	<xsl:param name="Pass">1</xsl:param>
	
	<xsl:choose>
		<xsl:when test="$Pass &lt;= 100">
			<xsl:call-template name="HundredDoors">
				<xsl:with-param name="Pass" select="$Pass + 1" />
				<xsl:with-param name="Doors">
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">1</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">2</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">3</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">4</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">5</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">6</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">7</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">8</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">9</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">10</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">11</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">12</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">13</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">14</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">15</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">16</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">17</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">18</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">19</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">20</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">21</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">22</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">23</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">24</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">25</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">26</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">27</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">28</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">29</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">30</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">31</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">32</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">33</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">34</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">35</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">36</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">37</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">38</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">39</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">40</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">41</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">42</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">43</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">44</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">45</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">46</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">47</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">48</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">49</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">50</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">51</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">52</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">53</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">54</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">55</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">56</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">57</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">58</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">59</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">60</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">61</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">62</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">63</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">64</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">65</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">66</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">67</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">68</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">69</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">70</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">71</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">72</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">73</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">74</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">75</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">76</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">77</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">78</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">79</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">80</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">81</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">82</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">83</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">84</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">85</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">86</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">87</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">88</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">89</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">90</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">91</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">92</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">93</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">94</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">95</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">96</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">97</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">98</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">99</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
					<xsl:call-template name="GetDoorValue"><xsl:with-param name="DoorNumber">100</xsl:with-param><xsl:with-param name="Pass" select="$Pass" /><xsl:with-param name="Doors" select="$Doors" /></xsl:call-template>
				</xsl:with-param>
			</xsl:call-template>
		</xsl:when>
		<xsl:otherwise>
			<xsl:value-of select="$Doors" />
		</xsl:otherwise>
	</xsl:choose>
	
</xsl:template>

<xsl:template name="GetDoorValue">
	<xsl:param name="DoorNumber" />
	<xsl:param name="Pass" />
	<xsl:param name="Doors" />
	
	<xsl:choose>
		<xsl:when test="$DoorNumber mod $Pass = 0">
			<xsl:value-of select="(number(substring($Doors, $DoorNumber, 1)) + 1) mod 2" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:value-of select="substring($Doors, $DoorNumber, 1)" />
		</xsl:otherwise>
	</xsl:choose>
	
</xsl:template>

```

